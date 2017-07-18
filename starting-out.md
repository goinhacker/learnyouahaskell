# 기본 연산자

하스켈도 스크립트 언어처럼 실행해볼 수 있는 REPL을 지원합니다. 터미널이나 [하스켈 공식 웹 사이트](https://www.haskell.org/)를 통해서 간단히 설치가 가능합니다.

```haskell
GHCi, version 6.8.2: http://www.haskell.org/ghc/  :? for help  
Loading package base ... linking ... done.  
Prelude>
```

설치 후, ghci라고 입력하시면 REPL안으로 진입합니다.

```haskell
**[terminal]
Prelude> :set prompt "ghci> "
**[prompt ghci> ]
```

위와 같은 명령어로 간단히 프롬프트를 바꿀 수 있습니다.

```haskell
**[terminal]
**[prompt ghci> ]**[command 2 + 15]
17
**[prompt ghci> ]**[command 49 * 100]
4900
**[prompt ghci> ]**[command 1892 - 1472]
420
**[prompt ghci> ]**[command 5 / 2]
2.5
```

위와 같이 더하기, 곱하기, 빼기, 나누기 연산을 지원하고, 기본적으로 앞에서 부터 실행됩니다. 연산자 우선순위 조정은 괄호를 사용해서 변경할 수 있습니다. \(수학과 비슷하지요\)

```haskell
**[terminal]
**[prompt ghci> ]**[command (50 * 100) - 4999]
1
**[prompt ghci> ]**[command 50 * 100 - 4999]
1
**[prompt ghci> ]**[command 50 * (100 - 4999)]
-244950
```

여기서 음수를 사용할때는 약간 주의할 부분이 있습니다.

```haskell
**[terminal]
**[prompt ghci> ]**[command 5 * -3]
<interactive>:3:1: error:
    Precedence parsing error
        cannot mix ‘*’ [infixl 7] and prefix `-' [infixl 6] in the same infix expression
**[prompt ghci> ]**[command 5 * (-3)]
-15
```

위와 같이 음수를 그냥 쓰면 에러가 나옵니다. 음수를 사용할 때는 항상 괄호와 함께 사용하는 것이 좋습니다.

```haskell
**[terminal]
**[prompt ghci> ]**[command True && False]
False
**[prompt ghci> ]**[command True && True]
True
**[prompt ghci> ]**[command False || True]
True
**[prompt ghci> ]**[command not False]
True
**[prompt ghci> ]**[command not (True && True)]
False
```

하스켈에서는 boolean값을 True, False로 표현하고, &&\(and\), \|\|\(or\), not\(java의 !와 같음\)를 사용할 수 있습니다.

```haskell
**[terminal]
**[prompt ghci> ]**[command 5 == 5]
True
**[prompt ghci> ]**[command 1 == 0]
False
**[prompt ghci> ]**[command 5 /= 5]
False
**[prompt ghci> ]**[command 5 /= 4]
True
**[prompt ghci> ]**[command "hello" == "hello"]
True
```

비교 연산자로 ==, /=\(Java의 !=과 같음\)를 사용하고 있습니다.

```haskell
**[terminal]
**[prompt ghci> ]**[command 5 + "11ama"]
<interactive>:4:1: error:
    • No instance for (Num [Char]) arising from a use of ‘+’
    • In the expression: 5 + "11ama"
      In an equation for ‘it’: it = 5 + "11ama"
**[prompt ghci> ]**[command 5 == True]
<interactive>:5:1: error:
    • No instance for (Num Bool) arising from the literal ‘5’
    • In the first argument of ‘(==)’, namely ‘5’
      In the expression: 5 == True
      In an equation for ‘it’: it = 5 == True
```

하스켈에서는 +, ==과 같은 연산자도 함수 입니다. 이렇게 양쪽에 입력 파라메터를 받는 함수들을 infix function이라고 부릅니다. 이런 함수에서 양쪽에 다른 타입이 오면 위와 같은 에러가 발생합니다.

```haskell
**[terminal]
**[prompt ghci> ]**[command 5 + 4.0]
9.0
```

위 명령에서 5는 integer 또는 float가 될 수 있습니다. 이 경우는 뒤에 4.0으로 float이 왔기 때문에 5는 float이 됩니다.

```haskell
**[terminal]
**[prompt ghci> ]**[command succ 8]
9
**[prompt ghci> ]**[command min 9 10]
9
**[prompt ghci> ]**[command max 100 101]
101
```

하스켈에서는 함수의 파라메터를 space로 구분합니다. 위의 경우, succ, min, max가 함수이고 각각의 함수 뒤에 오는 숫자들이 함수의 입력 파라메터 입니다. C언어의 succ\(8\), min\(9, 10\), max\(100, 101\)와 같다고 생각하시면 됩니다.

```haskell
**[terminal]
**[prompt ghci> ]**[command succ 9 + max 5 4 + 1]
16
**[prompt ghci> ]**[command (succ 9) + (max 5 4) + 1]
16
**[prompt ghci> ]**[command succ 9 * 10]
100
**[prompt ghci> ]**[command succ (9 * 10)]
91
```

이런 함수들 역시 괄호를 사용하여 실행 순서를 변경할 수 있습니다.

```haskell
**[terminal]
**[prompt ghci> ]**[command div 92 10]
9
**[prompt ghci> ]**[command 92 `div` 10]
9
```

div와 같이 두개의 입력 파라메터를 받는 함수는 infix function으로 사용할 수 있습니다. 본문에서는 div같은 경우, infix로 쓰는 것이 더 이해하기 편하다고 권장하고 있습니다.

```haskell
**[terminal]
**[prompt ghci> ]**[command succ (succ 4)]
6
```

하스켈에서 위와 같이 함수가 중첩으로 호출되는 경우는 C언어의 succ\(succ\(4\)\)와 같습니다.



