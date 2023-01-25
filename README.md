# vrml2scad

Translate VRML / WRL files to OpenSCAD.

Designed specifically for importing KiCAD PCB designs as references while
designing enclosures and mounting in OpenSCAD.

## Support

This probably doesn't support 100% of the VRML spec. I've only written it
to parse the features in the VRML files I've generated from KiCAD. If it
doesn't support your use case, you are welcome to open an issue or PR!

## Usage

```
$ cargo run --release <my_pcb.wrl >my_pcb.scad
```

Write the VRML file to stdin, and an OpenSCAD script will be written to the
output, which you probably want to redirect to a file.

Every `DEF` in the VRML file is a variable or a module (depending on the type).
To include it in an existing module, you can import it with the
[`use`][scad-use] statement. Then, instantiate the module corresponding to the
top-level node, which is the _last_ module defined in the generated OpenSCAD
file. (If it's from KiCAD, it's probably `TXFM_1`.) For example:

```
use <my_pcb.scad>

// ...

TXFM_1();
```

[scad-use]: https://en.wikibooks.org/wiki/OpenSCAD_User_Manual/Include_Statement

## Current Issues

The main problem right now is performance, both parsing of VRML and
loading in OpenSCAD.

I am currently experimenting with ways to optimize the 3D model before
translating, to hopefully improve OpenSCAD loading times.

There is also plenty of room to make the VRML parser more efficient (e.g.
reducing copies / heap allocations), but it will probably require a complete
rewrite of the parsing functions.
