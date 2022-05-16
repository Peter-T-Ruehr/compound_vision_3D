# compound_vision_3D
This repository contains
* R pacckage to automatically extract and manually correct 3D information (number of facets, facet positions, inter-facet angles, sensitivity and acuity) of compound eye STLs.

## Code available elsewhere (not yet published):
* ImageJ macro language code to
    * crop out the left and the right eye from a CT image stacks of various sources
    * crop out head capsule and automatically scale down for landmarking if necessary
* Python code to
  * extract compund eye surface from head capsule STL in Blender
* R code to 
  * analyse the local topological differences of facet sizes and inter-facet angles
  * calculate field of view (FOV) on local differences of acuity and sensitivity within FOV
  * compare these values in an phylogenetic comparative framework
  * analyse the trade-off between acuity and sensitivity across species

This is an alpha-version and still under development.

## Example output
### Direction and acuity
<img src="https://live.staticflickr.com/65535/52077372779_5dafd04018_o.gif" alt="species 1" width="600"/>

species 1

<img src="https://live.staticflickr.com/65535/52076138677_8fb88204ae_o.gif" alt="species 2" width="600"/>

species 2


### Field of view (FOV) and acuity
![species 1](https://live.staticflickr.com/65535/52076088442_1bff87d231_o.png)

species 1

### Interspecific variation: Facet size, inter-facet (IF) angle and sensitivity (P)
![species 1-11](https://live.staticflickr.com/65535/52077614450_71d1ecd3bc_o.png)

species 1-11
