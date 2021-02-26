### 5.0.0

_(in progress)_

DONE:

* One-Control example;
* Color button control;

TODO:

* New design;
* Helpers, optional to use, based on the examples, like `detachable`, `communicateJS`...
* Rename `Gui` module to `Tron`;
* Rename `Gui.Message` to `Gui.Msg` to be consistent with usual Elm style;
* Pagination in panels which don't fit the inner buttons;
* Fix URLs for icons not to require `assets/{icon}_{theme}.svg` address;
* Multiple-choice control;
* Include links to the examples in the docs;
* Include minimal "OneControl" example into the docs;


### 4.0.0

* Now there is a `Style.Shape` module that helps to define the shape of the plate in a friendly method, like only by specifying the required number of rows or columns (and no adjustment to cell shape is required anymore);
* Docking support for all nine directions;
* Now the whole viewport is used for the grid by default + let user request the custom shape of the grid;
* All the style modules are moved to `Style.*` package and properly separated;
* `Gui.over` function keeps the expanded panels expanded, the new `Gui.use` function—doesn't;
* `Builder.expand` + `Builder.collapse` helpers;
* Now controls' types and helpers are defined in the separate modules;
* `Builder.labels`;
* The Goose example instead of the boring default one (still, the boring one can be switched back);

### 3.0.0

* `Builder.map` and `Builder.Set`;

### 2.0.0

* Support cell shapes other than 1x1;
* Support different docks: `TopToBottom`, `BottomToTop`, `LeftToRight`, `RightToLeft`;
* Calculate the grid size dynamically;
* `Gui.redock` to allow to force custom size of the grid;
* _Choice_ component shows the selected item instead the expanding control;
* `Builder.choiceWithIcons` + `Builder.string` + `Builder.palette` helpers;
* Shape is required to be passed to group controls;
* _Button_ now may have different faces : icon, color, or just text;
* Fix _X/Y_ control to be exported to `dat.gui`;

### 1.0.1

* Detaching is based on Client IDs;
* `Render.Style` module is exposed separately;
* Design fixes;

### 1.0.0

* Basic controls and the examples are there:
    * _Knob_;
    * _XY_;
    * _Toggle_;
    * _Color_;
    * _Choice_;
    * _Nesting_;
    * _Button_;
    * _Text_;
* Detaching is supported;
* Converting tree to `dat.gui` and back is supported;
