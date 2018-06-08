type game = {
  score: int,
  isOver: bool,
  field: Field.field,
};

type fieldIterator = (Field.field, Field.position => unit) => unit;

let startGame = size => {
  score: 0,
  isOver: false,
  field:
    Field.populateRandomCell @@
    Field.populateRandomCell @@
    Field.makeEmptyField(size),
};

let mergeIfEqualNumber = (field, target, source) =>
  if (snd(target) == snd(source)) {
    Field.setCell(fst(target), field, Some(snd(target) * 2));
    Field.setCell(fst(source), field, None);
  };

let mergePhase = (searchDirection, field, position) =>
  switch (Field.getCell(position, field)) {
  | Some(cell) =>
    switch (Field.nextCellPosition(searchDirection, field, position)) {
    | Some(nextCell) =>
      mergeIfEqualNumber(field, (position, cell), nextCell)
    | None => ()
    }
  | None => ()
  };

let move = (game: game, iterator: fieldIterator, direction) : game =>
  if (game.isOver) {
    game;
  } else {
    iterator(
      game.field,
      mergePhase(Field.oppositeDirection(direction), game.field),
    );
    if (Field.hasEmptyCells(game.field)) {
      let _ = Field.populateRandomCell(game.field);
      game;
    } else {
      {...game, isOver: true};
    };
  };

let moveUp = game => {
  let iterator = (field, process) =>
    for (x in 0 to Field.(field.size) - 1) {
      for (y in 0 to Field.(field.size) - 1) {
        process({Field.x, y});
      };
    };
  move(game, iterator, Up);
};

let moveDown = game => {
  let iterator = (field, process) =>
    for (x in 0 to Field.(field.size) - 1) {
      for (y in game.field.size - 1 downto 0) {
        process({Field.x, y});
      };
    };
  move(game, iterator, Down);
};

let moveLeft = game => {
  let iterator = (field, process) =>
    for (y in 0 to Field.(field.size) - 1) {
      for (x in 0 to Field.(field.size) - 1) {
        process({Field.x, y});
      };
    };
  move(game, iterator, Left);
};

let moveRight = game => {
  let iterator = (field, process) =>
    for (y in 0 to Field.(field.size) - 1) {
      for (x in Field.(field.size) - 1 downto 0) {
        process({Field.x, y});
      };
    };
  move(game, iterator, Right);
};

Random.init(int_of_float(Js.Date.now()));