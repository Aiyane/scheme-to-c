将 Scheme 的一个小小子集编译为 C，该子集定义

```
Expr    ----> number                     ; 整数
          |   var                        ; 变量
          |   (zero? Expr)               ; 谓词
          |   (sub1 Expr)                ; 单目运算
          |   (* Expr₁ Expr₂)            ; 双目运算
          |   (let ((var Lambda)) Expr)  ; 函数绑定
          |   (if Expr₁ Expr₂ Expr₃)     ; 条件语句
          |   (letcc var Expr)           ; 获取 continuation
          |   (throw Expr₁ Expr₂)        ; 将 Expr₂ 值传入 Expr₁ 表示的 continuation 中
          |   Lambda                     ; 函数
          |   (Expr Expr)                ; 调用
Lambda  ----> (λ (var) Expr)
```

运行 `scheme pc2c.ss` 进入交互环境。在交互环境中输入

```scheme
(compile/run "test")
```

默认后缀是`.ss`，将 test.ss 文件中的 Scheme 程序编译生成同名的 C 文件与头文件，并使用 gcc 编译运行该 C 文件。注意一个文件中只处理一个 Expr。如果只需要得到 C 文件而不需要直接运行使用以下命令

```scheme
(pc2c "test.ss" "test.c" "test.h")
```

文件 `test.ss` 是一个例子。
