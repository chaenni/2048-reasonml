let return = a => Some(a);
let (>>=) = (m: option('a), f: 'a => option('b)) : option('b) =>
  switch (m) {
  | None => None
  | Some(a) => f(a)
  };