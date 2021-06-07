### 11.0.0

* Add `DeduceIn` as the message with label-path and JSON value to be deduced to current state of the tree, when sent;
* Rename `Exp.RawInUpdate` -> `Exp.In`, `Exp.RawOutUpdate` -> `Exp.Out`, `Exp.RawProperty` -> `Exp.Property`;

### 10.0.0

* Fix pagination issue with distributing items;
* The border around color controls;
* Add `SendReceiveJson` option with ability to send JSON;
* Add `Docker` argument to run a specific example;

### 9.0.1

* Fix layouting of the controls at the root position when they don't fit in viewport;

### 9.0.0

* Add modes for `Choice` control: now it can be a knob (changed by drag or switched by click, use `Builder.toKnob`) or just a click-through switch  (use `Builder.toSwitch`);
* Add _Live_ controls: the value is changing together with the updates, use `Builder.live` to convert any control to its live version;
* New pagination style;
* Add `Debug` option to render targets, which could help with observation of the problems with UI;
* Fix the major bug with `Path.pop` returning reverse result and, so, working with second-level panels;
* Significantly improve knobs/drag-driven-controls behaviour
    * Fix dragging in general;
    * Fix knobs to use current value when user starts dragging;
    * Fix/improve knobs to support on-the-fly change of the value;
    * Fix knobs showing the values `< min` or `> max`;
* Add the border to the expanded and/or focused items;
* Add the text mask to wide texts on controls so that they would overlflow the borders just slightly;
* Add `Tron.Random` UI generator to the API just for fun;
* `Tron.Expose.ProxyValue` -> `Tron.Control.Value`;
* Major changes in the core API, no more functions or messages are stored in the `Model`, `Tron` just assigngs some value to the control now, instead of storing a handler, but there's also `Tron.OfValue` from now on, which converts the control's value to the user `msg`;
* Get rid of `Tron.Builder.String`, it is not very useful and can easily be converted from `Tron ()` or `Tron Value`;

### 8.0.1

* New base font: `Oxanium`;
* Fix `.int` controls and knob controls in general to show the value b/w `min` and `max` instead of just `0..1` value;

### 8.0.0

* Remove previously required `PanelShape` and `CellShape` from `Builder.nest` and `Builder.choice` and make them separate functions `Builder.shape` & `Builder.cells` correspondigly;
* Add `Builder.face` and remove `Builder.buttonWith` so that there will be only one way to change the button/nest/choice face through API;
* `WithTron.Backed` helpers are now separate and improved and extended with the ones that have access to the value storage;
* `WithTron.ValueAt` as a public API module with helpers;
* Choice can optionally have a face (`icon` / `color`) which overrides showing selected option;
* Fixed: the size of the choice/nest can not be changed dynamically;
* Fixed: distribution over pages;
* Improve `Tron.palette` control rendering & handling (which is just choice over colors);
* Better icon size;
* Show labels on the buttons as well;
* Show values on the knobs;
* `Tron.palette` now asks for labels along with colors;
* Remove collapse panel button (since click on groups/choices expands anyway);
* `Gui.css` -> `Tron.css`;

### 7.0.0

* Make `Expose.Data.Convert` work with `Tron *` instances instead of `Property *`, which was confusing, though it is just the alias anyway.
* Fix `rows` & `cols` for `PanelShape`;
* Make `Builder` API friendlier with `choice` accepting a list of choices as configured controls, instead of just choices: i.e. buttons, with icons, if needeed, or just any control;
* Add `setIcon` and `setColor` to the `Builder` API so that `.nest` could have an icon instead of showing the arrow;
* Hide on _Space_ functionality;

### 6.0.0

* Expose `Tron.Option`;
* Move and rename `Builder msg` to `Tron msg` so that it will have more similarities to `Html msg`, `Svg msg` & s.o;
* Make `Backed` as JSON default helper, and the `Backed` as strings — secondary;

### 5.0.0

* `WithTron` helper that wraps `Browser.*` helpers and allows to add GUI to the existing Elm application easily;
* Much cleaner API of the `Builder` and others;
* New layouting system instead of `BinPack` used previously;
* Add pagination support for groups where number of controls inside exceeds some amount;
* Color button control;
* Examples for sending GUI updates to JS, in JSON or in Strings;
* Helpers, optional to use, based on the examples, like `detachable`, `communicateJS`...
* Rename `Gui` module to `Tron`;
* Fix URLs for icons not to require `assets/{icon}_{theme}.svg` address;
* Let user refer to icons using `Url` rather than filename and allow to be dependent on `Theme`;
* Hide `Tron` (previously `Gui`) API with `Msg`, `init`, `view`... and stuff, only expose the `WithTron` API;
* Remove unexpected sorting in nested panels;
* Separate `Dock` and its logic, `Expose` and its datatypes and logic, and hide such logic from user API;
* Fix `cols` vs `rows` to be bound to docking orientation (or just don't change direction of panels when docking);
* One-Control example;
* Make examples run easy with one script, through arguments;

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
* `Gui.reshape` to allow to force custom size of the grid;
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
