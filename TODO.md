## Public API

* Breaking: Close `ProxyValue` constructors from public;
    * Rename it to just `Value`, and put the definition into `Control` may be?
* Breaking: Stick to just one `WithTron.Backed`, since anyway it is possible to convert one to another using `Tron` methods;
* Breaking: Get rid of `maxRows` and `maxCols` in `PanelShape` and switch to manual pagination, do it only when user wants;
* Breaking, Bug: Adjust default values in knobs/XY to the actual range, or else it is rendered improperly;
* Ability to disable / enable pagination;
* Detachable: Add `clientId` to the URL when it was generated (so that reloading the page won't lose changes);
* Detachable: Hide the user view by default, when interface is detached;
    * Or give user the choice if to view the original model or not;
* Breaking: do not store `ClientID` in the `RawOutUpdate`, but be able to add it with `Expose.Convert` helpers and so use it only in `Detachable`, where it is needed;
* Breaking?: send some special value with `RawOutUpdate` for `Choice` controls or else it is hard to get what was actually chosen;
* Breaking?: for choice, give user option either to show icon on the button, or the label, even if the items in the choice have icons;
* Bug: selecting item on the second page of the choice control could make other panels content disappear; (Tiler: selecting _Tile/Tileset_ breaks _Color Scheme/BG Color_)
    * It seems `[0, 2]` and `[2, 0]` are conflicting in this case;

## UX / Design

* Show the value on XY controllers as well;
* Find a way to show long labels in two lines or scale the font or smth.;
* Test keyboard navigation, adapt it to the Dock;
* Fix knobs to use current value when user starts dragging;
* _Active_ condition for a button, some effect for when it is pressed;
* Fix/improve knobs to support on-the-fly change of the value;
* Vertical pagination;

## New controls

* Choice-by-click control — the button that changes state while it's clicked;
    * Maybe reuse `Nest.Form` for that;
* Multiple-choice control;
* Locked controls;
* Knob with fixed values, they could even labeled as strings (i.e. 3, 5, 7, 9, 15, _smooth_);

## Inner API / Logic

* Move `Util` stuff to the corresponding modules;
* Debug `RenderMode` (i.e. ensure `Debug` view still works);
* Use `Size Cells`, like integer size, in `Layout`;
* Breaking?: Use some safe Unique IDs to reference the position of the control in the tree, so that while the tree structure is changing, ID's stay the same;
    * Or, store such IDs together with property;
    * Consider having `Nil (Property msg)` instead of just `Nil`, so that any property could be hidden, but not absent in the tree;
    * Check `indexedMap` usages, so that usage of the index is kept to minimum for nested items (mostly done);
* Get rid of functions in the `Model`:
    * do not store tree in the `Gui msg`, build it every time;
    * store the actual messages for the current value in the controls, not the handlers (i.e. just `msg` instead of `v -> msg`);
        * or don't even store the messages, but only values, and only transform them to messages on the `view` stage;
        * ...like in the model it's `Tron ()`, but
    * for `.over`, traverse two trees with the same structure (don't forget about ghosts) and move transient states between them;
    * Breaking?: `Control`/`Tron`.`andThen` — due to handler and `Maybe`, now it is impossible to implement, so I did `Tron.with`;
    * Remove `evaluate__` functions;
    * Consider `Control setup msg value = Control (setup -> (Cmd msg, value))`
* Do not store cell size in the `Gui msg`, it should be recalculated every time;
* Do not store dock in the `Gui msg`, it should be recalculated every time;
* Breaking: Change choice and nest to work with `Array`s since we usually need to get item by index? But Array syntax is not very friendly for API

## Deployment

* For Docker, add ability to run any example using environment variable;

## Examples

* Add some indication of the WS server status to the examples;
* Include separate `Random` example to only utilize random generator, and, may be, test the detachable functionality, if the server is started;
* Include links to the examples in the docs;
* Share examples somewhere, i.e. deploy to github;
* A-Frame renderer & Demo to some senseful state;
