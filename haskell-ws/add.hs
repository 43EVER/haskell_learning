add::(Int, Int) -> Int
add (x, y) = x + y

f n = if n == 0 then 1 else n * f (n - 1)

add' x y = x + y