type element;
let document: element = [%bs.raw "document"];

[@bs.send]
external addKeyboardEventListener :
  (element, string, ReactEventRe.Keyboard.t => unit) => unit =
  "addEventListener";

type state = {game: Game.game};

type action =
  | MoveUp
  | MoveDown
  | MoveLeft
  | MoveRight
  | DoNothing;

let component = ReasonReact.reducerComponent("Game");

let mapKeyCodeToAction = (keyCode: int) : action =>
  switch (keyCode) {
  | 37 => MoveLeft
  | 38 => MoveUp
  | 39 => MoveRight
  | 40 => MoveDown
  | _ => DoNothing
  };

let renderCell = (cell: Game.cell) =>
  switch (cell) {
  | EmptyCell => <div className="cell empty-cell" />
  | NumberedCell(value) =>
    <div className="cell number-cell">
      <span className="number">
        (ReasonReact.string(string_of_int(value)))
      </span>
    </div>
  };

let make = _children => {
  ...component,
  initialState: () => {game: Game.startGame(4)},
  didMount: self =>
    addKeyboardEventListener(document, "keydown", event =>
      event |> ReactEventRe.Keyboard.keyCode |> mapKeyCodeToAction |> self.send
    ),
  reducer: (action, state) =>
    switch (action) {
    | MoveUp => ReasonReact.Update({game: Game.moveUp(state.game)})
    | MoveDown => ReasonReact.Update({game: Game.moveDown(state.game)})
    | MoveLeft => ReasonReact.Update({game: Game.moveLeft(state.game)})
    | MoveRight => ReasonReact.Update({game: Game.moveRight(state.game)})
    | DoNothing => ReasonReact.NoUpdate
    },
  render: self => {
    let field = self.state.game.field.field;
    <div className="game-grid">
      (Array.map(renderCell, field) |> ReasonReact.array)
    </div>;
  },
};