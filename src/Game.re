let base = 2;

type cell =
  | NumberedCell(int)
  | EmptyCell;

type field = {
  field: array(cell),
  size: int,
};

type game = {
  score: int,
  isOver: bool,
  field,
};

type position = {
  x: int,
  y: int,
};

module Field = {
  let randomNewTileValue = () => Random.int(100) < 90 ? 2 : 4;

  let makeEmptyField = (size: int) : field => {
    field: Array.make(size * size, EmptyCell),
    size,
  };

  let indexedEmptyCells = (field: field) =>
    field.field
    |> Array.to_list
    |> List.mapi((index, cell) => (index, cell))
    |> List.filter(((_index, cell)) => cell == EmptyCell);

  let randomEmptyPosition = (field: field) => {
    let emptyCells: list((int, cell)) = indexedEmptyCells(field);
    fst @@ List.nth(emptyCells, Random.int(List.length(emptyCells)));
  };

  let populateRandomCell = (field: field) => {
    let randomPosition = randomEmptyPosition(field);
    field.field[randomPosition] = NumberedCell(randomNewTileValue());
    field;
  };

  let indexFromPosition = (position: position, fieldSize: int) : int =>
    position.x * fieldSize + position.y;

  let getCell = (position: position, field: field) : cell => {
    let index = indexFromPosition(position, field.size);
    field.field[index];
  };

  let setCell = (position: position, field: field, value: cell) => {
    let index = indexFromPosition(position, field.size);
    field.field[index] = value;
    ();
  };
};

let startGame = size => {
  score: 0,
  isOver: false,
  field:
    Field.populateRandomCell @@
    Field.populateRandomCell @@
    Field.makeEmptyField(size),
};

let moveLeft = game => {
  Js.log("left");
  game;
};

let moveRight = game => {
  Js.log("right");
  game;
};

let moveUp = game => {
  Js.log("up");
  game;
};

let moveDown = game => {
  Js.log("down");
  game;
};

Random.init(int_of_float(Js.Date.now()));