* Choice-by-click control — the button that changes state while it's clicked;
    * Maybe reuse `Nest.Form` for that;
* Breaking: Close `ProxyValue` constructors from public;
    * Rename it to just `Value`, and put the definition into `Control` may be?
* Test keyboard navigation, adapt it to the Dock;
* Fix knobs to use current value when user starts dragging;
* Breaking: For `Backed` Tron, give user access to the values in the dictionary and the easy way to parse them;
    * It is needed, for the cases when parts of the GUI are hidden when some switch is off, like in the case of Goose example;
    * On the other hand, the current values could be overwritten from the JSON dict, think it over;
* FIXME: the size of the choice/nest can not be changed dynamically — it takes the size which was set at first;
    * See `ForTiler` example, we are required to know the whole list of the tiles before;
* Move `Util` stuff to the corresponding modules;
* Debug `RenderMode`;
* Use `Size Cells`, like integer size, in `Layout`;
* _Active_ condition for a button, some effect for when it is pressed;
* For Docker, add ability to run any example using environment variable;
* Detachable: Add clientId to the URL when it was generated (so that reloading the page won't lose changes);
* Detachable: Hide the user view by default, when interface is detached;
* New design;
* Multiple-choice control;
* Locked controls;
* Breaking: Make `setIcon` and `buttonWith` replaceable or just join into one function;
* Include links to the examples in the docs;
* Share examples somewhere, i.e. deploy to github;
* Breaking?: When in the detached state, give user the choice if to view the original model or not (or just always hide it, in detached mode);
* Include separate `Random` example to only utilize random generator, and, may be, test the detachable functionality, if the server is started;
* Breaking: Use some safe Unique IDs to reference the position of the control in the tree, so that while the tree structure is changing, ID's stay the same;
    * Or, store such IDs together with property;
* Fix distribution over pages (use `Pages.distributeOver pageCount` instead of `Pages.distribute 9`);
* Consider having `Nil (Property msg)` instead of just `Nil`, so that any property could be hidden, but not absent in the tree;
* Get rid of functions in the `Model`:
    * do not store tree in the `Gui msg`, build it every time;
    * store the actual messages for the current value in the controls, not the handlers (i.e. just `msg` instead of `v -> msg`);
    * for `.over`, traverse two trees with the same structure (don't forget about ghosts) and move transient states between them;
    * Breaking?: `Control`/`Tron`.`andThen` — due to handler and `Maybe`, now it is impossible to implement, so I did `Tron.with`;
    * Remove `evaluate__` functions;
    * Consider `Control setup msg value = Control (setup -> (Cmd msg, value))`
* Check `indexedMap` usages, so that usage of the index is kept to minimum for nested items (mostly done);
* Do not store cell size in the `Gui msg`, it should be recalculated every time;
* Do not store dock in the `Gui msg`, it should be recalculated every time;
* Breaking: do not store `ClientID` in the `RawOutUpdate`, but be able to add it with `Expose.Convert` helpers and so use it only in `Detachable`, where it is needed;
* Add some indication of the WS server status to the examples;
* With choice, also send the value chosen;
* Breaking: Change choice and nest to work with `Array`s since we usually need to get item by index? But Array syntax is not very friendly for API
