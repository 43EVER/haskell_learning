# Day3 Haskell —— λ calculus 学习

终于搞懂了 λ 里，Y 组合子的作用，果然学习的最好方法就是踩坑，自己跟着博客实现一遍，感觉清楚了很多，好了，再到博客里实现一遍，加强记忆

metafact = lambda fact . lambda x . x == 1 ? 1 : x * (fact fact) (x - 1)

(metafact metafact) 5

实现了递归，但不够优雅

1. w = (f f)
2. (w metafact) 5
3. 公式里还有 fact fact 这种东西，不够直观，换成 g
4. metafact = lambda fact . ((lambda g . lambda x . x == 1 ? 1 : x * g(x - 1))) (fact fact))
5. 中间那一坨就是我想要的递归函数应该有的样子，抽出来 叫 f0
6. metafact = lambda fact . (f0 (fact fact))
7. [fact / h] metafact = lambda h . (f0 (h h))
8. 展开w
9. fact = ((lambda h . f0 (h h)) (lambda h . f0 (h h)))
10. f0 从外部传进去
11. fact = ( lambda f ((lambda h . f (h h)) (lambda h . f (h h))) ) (lambda g . lambda n . n == 1 ? 1 : n * g (n - 1))
12. 前面那一坨就是 提出来，叫 Y
13. Y = lambda f ((lambda h . f (h h)) (lambda h . f (h h)))
14. [h / x]
15. Y = lambda f ((lambda x . f (x x)) (lambda x . f (x x)))

真应该去好好搞搞 latex，这样写出来是真的丑

好了，上面就从穷人的Y组合子，一步一步抽象，就是把业务逻辑全部提出了来，然后得到了真正的 Y，下面应该思考 Y 究竟是干嘛的

lambda calculus 以下简称 LC.

LC. 里没有函数名这个玩意，但平常还好，但我想递归的时候就完蛋，所以我需要整个东西出来，来让我得到我的递归函数

那么我可以设计一个函数，G ，这个函数满足 G(f) = f，而且定义里不出现 f，这样我就实现了 递归函数

这个 G 很容易写，因为 f 都传进来了，那么我用参数接住就行了嘛

lambda f . lambda x . x == 1 ? 1 : x * f(x - 1)

大哥们还规定了一个东西，当 G(f) = f 时，f 叫这个函数的不动点，那么当我需要这个函数的时候，去求 G 的不动点就行了，即 G(Y G)

Y 就是干这个事的，Y 对任意函数，返回值都是这个函数的不动点

如果我平常想写递归函数，就不用 f = G (Y G)

(Y G) 求的是 G 的不动点，那么我还套层 G 干啥，直接 (Y G) 就好了



以上就是 Y 的用途和 Y 怎么弄出来的，花了两天，终于搞懂了。

要说学到了什么的话，就是不要怕掉坑里，如果凭自己的智商理解不了的话，那就跳坑里，坑里待久了，概念记得差不多，看两篇博客马上就通了。





