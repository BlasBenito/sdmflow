# TO DO

## v_{} functions

Functions to deal with raster data representing environmental information

### **import4D**: rename to **v_import**
Changes in arguments:

+  raster.template -> rename to v.template
+  raster.template.crs -> rename to v.template.crs
+  folder -> rename to v.folder
+  dynamic.vars -> rename to v.dynamic
+  static.vars -> rename to v.static
+  times -> rename to v.times
+  vars.crs -> rename to
+  to.data.frame -> remove this option and keep it in a helper function

### **importTIF**: include in **v_import**

### **importASC**: include in **v_import**





## o_{} functions

Functions to deal with occurrence data

### **o_make_training()**
Changes in argumets:

+  variables -> find proper name for this argument after v_{} functions are ready
+  n -> rename to n.background
+  presence.only -> remove this option
+  background -> remove this option
+  restricted.background -> remove this option, link it to non-null restricted.background.buffer
+  restricted.background.buffer -> restriction.radius? restrict.to.buffer?
+  thinning -> remove this option, link it to non-null minimum.distance
+  minimum.distance -> rename to same as in o_thinning(), maybe min.dist


### **o_thinning()**

This function should be paired with the function to test spatial correlation (testSpatialCorrelation()). Maybe o_thinning() should be s_thinning() and testSpatialCorrelation() should be s_spatial_cor(), so if there are variables with a high spatial correlation for the presences, the user can apply thinning and/or remove the offending variables. This should be done most likely before assessing multicollinearity.

Changes in arguments:

+  variables -> find proper name for this argument after v_{} functions are ready
+  minimum.distance -> rename to min.dist
+  random.start -> remove this option, link to a non-null random.seed
+  seed -> rename to random.seed


## s_{} functions

Functions for variable selection and reduction of multicollinearity
