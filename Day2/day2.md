# λ演算 学习

如何理解 Y ？

​	Y 是一个特殊的高阶函数

​		什么是高阶函数？

​			高阶函数是指一类这样的函数：接受函数作为参数，返回值也是一个函数

​		Y 为什么特殊？

​			Y 是一种组合子

​				什么是组合子？

​					一类特殊的高阶函数，他们只引用 函数应用（函数调用）

​			let Y = lambda y . ( lambda x . y (x x) ( lambda x . y (x x) ) )

​				Y 可以应用自身来创造本身，即 (Y Y) = Y (Y Y)

​	(Y Y)

   1. 展开第一个 Y ： (lambda y . (lambda x . y (x x)) (lambda x . y (x x))) Y

   2. beta规约 ：(lambda x . Y (x x)) (lambda x. Y (x x))

   3. alpha[x/z] 变换第二个 lambda ：(lambda x. Y (x x)) (lambda z. Y (z z))

   4. beta规约 ：Y ((lambda z. Y (z z)) (lambda z. Y (z z)))

   5. 展开前面的 Y 并 alpha[y/a] [x/b]：lambda a . (lambda b . a (b b) (lambda b a (b b))) ((lambda z. Y (z z)) (lambda z. Y (z z)))

   6. beta规约 ：(lambda b . ((lambda z. Y (z z)) (lambda z. Y (z z))) (b b)) (lambda b ((lambda z. Y (z z)) (lambda z. Y (z z))) (b b))

   7. 化简一哈 ：把 lambda z . Y (z z) 看成 A，就变成了 

      (lambda b . (A A) (b b)) (lambda b . (A A) (b b))

      (Y Y) = (lambda y . (lambda x . y (x x)) (lambda x . y (x x))) Y

      = (lambda x . Y (x x)) (lambda x . Y (x x))

      (Y (Y Y)) = lambda y . (lambda x . y (x x) (lambda x . y (x x))) ((lambda x . Y (x x)) (lambda x . Y (x x)))

      = (lambda x ((lambda x . Y (x x)) (lambda x . Y (x x))) (x x)) (lambda x ((lambda x . Y (x x)) (lambda x . Y (x x))) (x x))

			8. 总结一下，化简部分那块省略了几个 alpha变换，只会从 结论 推回去，

let metafact = lambda fact . (lambda n . n == 0 ? 1 : n * fact (n - 1))

metafact (Y metafact)，总之证明了 (Y Y) = (Y (Y Y))



所以 Y 的作用是什么？

​	首先是 lambda 演算里没有函数名，而我递归需要函数名 f，这就出问题了。

​	这个时候有一个解决方法，我再来个函数 G ，它接收一个函数 fuck，并且返回一个函数 f，返回的函数 会判断递归终点，没到就调用fuck继续递归

``` cpp
auto G = [](function<int(int)> fuck) {
    return [&](int x) {
        return x == 0 ? 1 : x * fuck(x - 1)
    }
}
```

如果，正好有一个函数 fuck ，使得

```cpp
G(fuck) == [](int x) {
    return x == 0 ? 1 : x * fuck(x - 1)
} == fuck
```

这个时候，fuck 就是我们需要的匿名函数 factorial

G(fuck) = fuck，这个 fuck 就是所谓的不动点，这个是时候，如果我需要一个递归函数，我只需要把 G 这个函数的不动点求出就好了

那么问题就变成了，怎么求出 G 这个函数，怎么求出 G 的不动点

1. G 怎么求？G 其实就是一个高阶函数，他接收另一个函数 fuck，返回一个函数 f，返回的这个函数会判断递归终点，没到就调用 fuck 继续递归
2. 不动点怎么求，这就是 Y 的作用

假设有一个递归函数，求阶乘的，那么应该这样求

factorial = YG



factorial = YG 使得 G(factorial) = factorial

证明：对于任意 G, G (Y G) = (Y G)

令 W = lambda x . G (x x), X = (W W)

有 X = (W W) = ((lambda x . G (x x)) W) = G (W W)

又因为 (Y G) = (lambda y . ( lambda x . y (x x) ) ( lambda x . y (x x))) G = (lambda x . G (x x)) (lambda x G (x x)) = (W W) = X

所以 G X = X 就是 G (Y G) = (Y G)





