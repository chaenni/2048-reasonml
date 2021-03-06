open MaybeMonad;

type position = {
  x: int,
  y: int,
};

type direction =
  | Left
  | Right
  | Up
  | Down;

let movePosition = (direction: direction, {x, y}: position) =>
  switch (direction) {
  | Left => {x: x - 1, y}
  | Right => {x: x + 1, y}
  | Up => {x, y: y - 1}
  | Down => {x, y: y + 1}
  };

let oppositeDirection = (direction: direction) =>
  switch (direction) {
  | Left => Right
  | Right => Left
  | Up => Down
  | Down => Up
  };

type cellValue = int;

type cell = option(cellValue);

type field = {
  field: array(cell),
  size: int,
};

type positionedCell = (position, cellValue);

let randomNewTileValue = () => Random.int(100) < 90 ? 2 : 4;

let makeEmptyField = (size: int) : field => {
  field: Array.make(size * size, None),
  size,
};

let indexedEmptyCells = (field: field) =>
  field.field
  |> Array.to_list
  |> List.mapi((index, cell) => (index, cell))
  |> List.filter(((_index, cell)) => cell == None);

let hasEmptyCells = (field: field) =>
  List.length(indexedEmptyCells(field)) > 0;

let randomEmptyPosition = (field: field) => {
  let emptyCells: list((int, cell)) = indexedEmptyCells(field);
  fst @@ List.nth(emptyCells, Random.int(List.length(emptyCells)));
};

let populateRandomCell = (field: field) => {
  let randomPosition = randomEmptyPosition(field);
  field.field[randomPosition] = Some(randomNewTileValue());
  field;
};

let indexFromPosition = (position: position, fieldSize: int) : int =>
  position.y * fieldSize + position.x;

let getCell = (position: position, field: field) : cell => {
  let index = indexFromPosition(position, field.size);
  field.field[index];
};

let setCell = (position: position, field: field, value: cell) => {
  let index = indexFromPosition(position, field.size);
  field.field[index] = value;
  value;
};

let isInField = ({size}: field, {x, y}: position) =>
  x >= 0 && x < size && y >= 0 && y < size;

let rec traverseToNextNumberedCell =
        (direction: direction, field: field, position: position)
        : option(positionedCell) =>
  if (isInField(field, position)) {
    switch (getCell(position, field)) {
    | Some(cellValue) => Some((position, cellValue))
    | None =>
      traverseToNextNumberedCell(
        direction,
        field,
        movePosition(direction, position),
      )
    };
  } else {
    None;
  };

let rec furthestEmptyPosition =
        (direction: direction, field: field, position: position) =>
  if (isInField(field, position)) {
    switch (getCell(position, field)) {
    | Some(_) => None
    | None =>
      switch (
        furthestEmptyPosition(
          direction,
          field,
          movePosition(direction, position),
        )
      ) {
      | Some(foundPosition) => return(foundPosition)
      | None => return(position)
      }
    };
  } else {
    None;
  };

let nextCellPosition =
    (direction: direction, field: field, position: position)
    : option(positionedCell) =>
  traverseToNextNumberedCell(
    direction,
    field,
    movePosition(direction, position),
  );

let nextFurthestEmptyPosition =
    (direction: direction, field: field, position: position) =>
  furthestEmptyPosition(direction, field, movePosition(direction, position));

module Iterator = {
  type iterator = (field, position => unit) => unit;

  let rightToLeft: iterator =
    (field, process) =>
      for (y in 0 to field.size - 1) {
        for (x in field.size - 1 downto 0) {
          process({x, y});
        };
      };

  let leftToRight: iterator =
    (field, process) =>
      for (y in 0 to field.size - 1) {
        for (x in 0 to field.size - 1) {
          process({x, y});
        };
      };

  let bottomUp: iterator =
    (field, process) =>
      for (x in 0 to field.size - 1) {
        for (y in field.size - 1 downto 0) {
          process({x, y});
        };
      };

  let topDown: iterator =
    (field, process) =>
      for (x in 0 to field.size - 1) {
        for (y in 0 to field.size - 1) {
          process({x, y});
        };
      };

  let forDirection = (direction: direction) : iterator =>
    switch (direction) {
    | Left => leftToRight
    | Right => rightToLeft
    | Up => topDown
    | Down => bottomUp
    };
};