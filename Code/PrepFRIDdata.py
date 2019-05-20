# Prep data for creating FRID summaries by forest and district

# Import system modules
import arcpy
arcpy.env.overwriteOutput = True

################################################################################################
# Declare all input and output file paths
# Folder containing FRID data:
arcpy.env.workspace = "C:/GIS/FRID17"

# Name the FRID layers to be merged
FRID1 = "FRID_CentralValley17_1.gdb/FRID_CentralValley17_1"
FRID2 = "FRID_CentralCoast17_1.gdb/FRID_CentralCoast17_1"
FRID3 = "FRID_SouthInterior17_1.gdb/FRID_SouthInterior17_1"
FRID4 = "FRID_SouthCoast17_1.gdb/FRID_SouthCoast17_1"

ProvBoundary = "C:/GIS/SoCal_Province_Boundary.shp"
BasicOwnership = "C:/GIS/BasicOwnership_SoCalClip.shp"
Districts = "C:/GIS/RangerDistrictsSoCalClip.shp"

# Name and path for output:
Outcsv = "C:/Users/krenwick/Documents/PMF/Details/SoCal/FRID_summaries/FRIDout.csv"
###############################################################################################

# Step 1: merge all FRID data for the current year
arcpy.Merge_management(inputs=[FRID1,FRID2,FRID3,FRID4], output="C:/GIS/FRID17/FRID_merge.shp")

# Step 2: clip FRID to just the SoCal province boundary
arcpy.Clip_analysis(in_features="FRID_merge.shp", clip_features=ProvBoundary, out_feature_class="FRID_merge_clip.shp")

# Step 3: intersect FRID with basic ownership and district boundary layers so all attributes together
t1 = time.time()
infeatures = [BasicOwnership, Districts, "FRID_merge_clip.shp"]
arcpy.Intersect_analysis(in_features=infeatures, out_feature_class="FRID_ownership.shp", join_attributes="ALL", output_type="INPUT")
t2 = time.time()

# Step 4: Calculate area of each new polygon
t4 = time.time()
arcpy.AddGeometryAttributes_management(Input_Features="FRID_ownership.shp", Geometry_Properties="AREA_GEODESIC", Area_Unit="ACRES")

# Step 5: export csv for analysis in R
arcpy.CopyRows_management("FRID_ownership.shp", Outcsv)
t5 = time.time()