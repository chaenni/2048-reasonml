open MaybeMonad;

type game = {
  score: int,
  isOver: bool,
  field: Field.field,
};

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
    let _ = Field.setCell(fst(source), field, None);
    Field.setCell(fst(target), field, Some(snd(target) * 2));
  } else {
    None;
  };

let mergePhase = (searchDirection, field, position) => {
  let _ =
    Field.getCell(position, field)
    >>= (
      cell =>
        Field.nextCellPosition(searchDirection, field, position)
        >>= (
          nextCell => mergeIfEqualNumber(field, (position, cell), nextCell)
        )
    );
  ();
};

let move = (game: game, direction: Field.direction) : game =>
  if (game.isOver) {
    game;
  } else {
    Field.Iterator.forDirection(
      direction,
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

Random.init(int_of_float(Js.Date.now()));