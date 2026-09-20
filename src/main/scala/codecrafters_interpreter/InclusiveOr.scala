package codecrafters_interpreter

enum InclusiveOr[A, B]:
  case Left(a: A)
  case Right(b: B)
  case Both(a: A, b: B)
