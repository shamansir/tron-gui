## Public API

* Breaking: Close `ProxyValue` constructors from public;
* Breaking: Stick to just one `WithTron.Backed`, since anyway it is possible to convert one to another using `Tron` methods;
* Get rid of `maxRows` and `maxCols` in `PanelShape` and switch to manual pagination, do it only when user wants: ability to disable / enable pagination from builder + Move paging inside nested controls;
* Detachable: Add `clientId` to the URL when it was generated (so that reloading the page won't lose changes);
* Detachable: Hide the user view by default, when interface is detached;
    * Or give user the choice if to view the original model or not;
* Breaking: do not store `ClientID` in the `RawOutUpdate`, but be able to add it with `Expose.Convert` helpers and so use it only in `Detachable`, where it is needed; (Partly done);
* Breaking: API shouldn't allow `nest` and `choice` with no items;
* Breaking?: send some special value with `RawOutUpdate` for `Choice` controls or else it is hard to get what was actually chosen;
* Breaking?: for choice, give user option either to show icon on the button, or the label, even if the items in the choice have icons;
* Bug: selecting item on the second page of the choice control could make other panels content disappear; (Tiler: selecting _Tile/Tileset_ breaks _Color Scheme/BG Color_)
    * It seems `[0, 2]` and `[2, 0]` are conflicting in this case;
* `Builder.noPaging`

## UX / Design

* Show the value on XY controllers as well;
* Test keyboard navigation, adapt it to the Dock;
* _Active_ condition for a button, some effect for when it is pressed;
* Vertical pagination;
* Consider selecting the page which current item when update came from JS;
* Support touch events;
* Nostalgic theme;

## New controls

* Multiple-choice control;
* Locked controls;

## Inner API / Logic

* Tests;
    * Detachable;
    * Sending updates from/to JS;
    * ...
* Bug: sending value from JS to the choice is not switching it to the corresponding page;
* Store pages inside nesting controls, do not redistribute every time;
* Max cols / Max rows should not be needed;
* Abstract `Layout.view` to `Html ((Path, Maybe a) -> Msg)`:
    * Render Text inputs separately for that to work: texts are the only controls that don't react on click rather on input;
    * Or, do it as `Layout.view : ((Path, Property a) -> Bounds -> ... -> msg)` and pass controls rendering functions there;
* Bug: applying updates as several packages from JS gives no effect (see `ForTiler` example);
* Detachable mode needs more testing;
* Bug: `toSwitch` is not sending proper events to JS;
* Move keyboard & mouse drag-start/dragging/drag-end logic to controls themselves;
* Move all possible control-related logic to the controls themselves;
    * Such logic can be found in code by adding some fake `()`-control and checking the places where we have to cover it / compiler fails;
    * Also can be found by closing `Property` and `Control` constructors from exposing;
* Move `Util` stuff to the corresponding modules;
* Move functions related to controls to the controls themselves, hide the `Control` constructor from others;
* Debug `RenderMode` (i.e. ensure `Debug` view still works);
* Use `Size Cells`, like integer size, in `Layout`;
* `Layout.pack` should also put `a` (from a `Property`) into every cell;
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
* Rename `Property` -> `Tree` or smth;

## Deployment

* For Docker, add ability to run any example using environment variable;

## Examples

* UI Constructor;
* Add some indication of the WS server status to the examples;
* Include separate `Random` example to only utilize random generator, and, may be, test the detachable functionality, if the server is started;
* Include links to the examples in the docs;
* Share examples somewhere, i.e. deploy to github;
* A-Frame renderer & Demo to some senseful state;
