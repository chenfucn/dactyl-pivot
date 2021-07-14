# The Dactyl-Pivot Keyboard

This is a fork of [Dactyl Manuform](https://github.com/abstracthat/dactyl-manuform) keyboard,
which in-turn is a fork of the [Dactyl Keyboard](https://github.com/adereth/dactyl-keyboard)

The Dactyl-Pivot adopts an open frame and multi-part design that affords the following
benefits:

* A pivoting thumb cluster.
* Adjustable tenting.
* No more hollowing sound caused by the big case.
* Greater flexibility to key placement. We don't need to worry about
installation clearance.
* 3d printing of these cases takes less plastic, reducing wastes.
* When customize the design, each part can be experimented on
seperatedly, leading to faster prototyping and less wastes.  

### Thumb cluster

A common complain aginst dactyl is the thumb cluster being too high. The popular manuform 
variant changed to a thumb cluster that contours downward. Unfortunately, this change
creates another problem, the thumb keys are too far away from the home row, leading to
lots of thumb stretching.

In normal keyboards, the thumbs rest on the space bar, which is only one row below the
home row. Comparing with the original dactyl, the manuform variant's thumbs keys are
much further away from the home row.

To solve this problem and reduce thumb stretching, the Dactyl-Pivot mixed the two
thumb clusters, where the thumb keys are closer to the home row while contouring
downward.

 

### Generating a Design

**Setting up the Clojure environment**
* [Install the Clojure runtime](https://clojure.org)
* [Install the Leiningen project manager](http://leiningen.org/)
* [Install OpenSCAD](http://www.openscad.org/)

**Generating the design**
* Run `lein repl`
* Load the file `(load-file "src/dactyl_pivot/dactyl.clj")`
* This will regenerate the `things/*.scad` files
* Use OpenSCAD to open a `.scad` file.
* Make changes to design, repeat `load-file`, OpenSCAD will watch for changes and rerender.
* When done, use OpenSCAD to export STL files

**Tips**
* [Some other ways to evaluate the clojure design file](http://stackoverflow.com/a/28213489)
* [Example designing with clojure](http://adereth.github.io/blog/2014/04/09/3d-printing-with-clojure/)


## License

Copyright © 2015 Matthew Adereth

The source code for generating the models (everything excluding the [things/](things/) and [resources/](resources/) directories is distributed under the [GNU AFFERO GENERAL PUBLIC LICENSE Version 3](LICENSE).  The generated models and PCB designs are distributed under the [Creative Commons Attribution-ShareAlike License Version 4.0](LICENSE-models).
