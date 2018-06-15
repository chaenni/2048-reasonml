type element;
let document: element = [%bs.raw "document"];

[@bs.send]
external addKeyboardEventListener :
  (element, string, ReactEventRe.Keyboard.t => unit) => unit =
  "addEventListener";

type state = {game: Game.game};

type action =
  | Move(Field.direction)
  | DoNothing;

let component = ReasonReact.reducerComponent("Game");

let mapKeyCodeToAction = (keyCode: int) : action =>
  switch (keyCode) {
  | 37 => Move(Left)
  | 38 => Move(Up)
  | 39 => Move(Right)
  | 40 => Move(Down)
  | _ => DoNothing
  };

let renderCell = (cell: Field.cell) =>
  switch (cell) {
  | None => <div className="cell empty-cell" />
  | Some(value) =>
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
    | Move(direction) =>
      ReasonReact.Update({game: Game.move(state.game, direction)})
    | DoNothing => ReasonReact.NoUpdate
    },
  render: self => {
    let field = self.state.game.field.field;
    let loseMessage =
      self.state.game.isOver ?
        <h2> (ReasonReact.string("You lost!")) </h2> : ReasonReact.null;
    <div>
      loseMessage
      <div className="game-grid">
        (Array.map(renderCell, field) |> ReasonReact.array)
      </div>
    </div>;
  },
};