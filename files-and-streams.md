
## Files와 Streams

### getContents

`getContents` 함수는 표준 입력으로부터 파일의 끝을 만날때까지 전부 읽어들이는 I/O 작업입니다. 이 함수의 타입은 `getContents :: IO String` 입니다. 이 `getContents` 함수의 가장 큰 장점은 게으른(lazy) I/O라는 점 입니다. 따라서 호출된 시점에 읽어들이지 않고, 값이 정말 필요한 시점에 콘솔에서 입력을 읽을 것입니다.

`getContents`는 어떤 프로그램의 출력은 다른 프로그램에 입력을 넣는 파이프라이닝을 할때 매우 유용한 함수입니다. 여기서는 유닉스 시스템에서 파이프라인 작업이 어떻게 동작하는지 살펴보겠습니다. 먼저 아래와 같은 텍스트 파일을 만들었습니다. 

```
I'm a lil' teapot  
What's with that airplane food, huh?  
It's so small, tasteless
```

이제는 이전에 소개드렸던 `forever` 함수를 사용해서 아래와 같은 프로그램을 만들었습니다. 

```haskell
import Control.Monad  
import Data.Char  
  
main = forever $ do  
    putStr "Give me some input: "  
    l <- getLine  
    putStrLn $ map toUpper l
```

이 프로그램은 사용자에게 한 라인을 받아서 대문자로 바꾸는 작업을 계속 반복합니다. 이 프로그램을 컴파일하고 위에서 만든 텍스트 파일을 넣어보면 아래와 같이 동작합니다.

```haskell
**[terminal]
**[prompt $ ]**[command ghc --make capslocker]
[1 of 1] Compiling Main             ( capslocker.hs, capslocker.o )
Linking capslocker ...
**[prompt $ ]**[command cat haiku.txt]
I'm a lil' teapot  
What's with that airplane food, huh?  
It's so small, tasteless
**[prompt $ ]**[command cat haiku.txt | ./capslocker]
I'M A LIL' TEAPOT  
WHAT'S WITH THAT AIRPLANE FOOD, HUH?  
IT'S SO SMALL, TASTELESS
capslocker <stdin>: hGetLine: end of file
```

여기서는 유닉스의 `cat`과 `|`를 사용해서 위에서만든 capslocker 프로그램에 전달하였습니다. 이것은 콘솔에서 capslocker를 실행하고 콘솔에서 텍스트 파일을 입력하고, EOF를 발생시키기 위해서 Ctrl-D를 누르는 작업과 동일합니다. 여기서 `cat haiku.txt | ./capslocker`는 바로 터미널에 출력되지않고, capslocker 프로그램에 전달되었습니다. 

`forever`를 사용할때는 필수적으로 입력을 받아서 출력으로 변환해야 합니다. 이러한 작업을 간단하게 처리하기 위해서 `getContents`를 사용합니다. 

```haskell
import Data.Char  
  
main = do  
    contents <- getContents  
    putStr (map toUpper contents)
```

이 예제는 `getContents` 수행해서 받은 결과를 `contents`에 바인딩 하였습니다. 그리고 나서 `toUpper` 함수를 사용해서 대문자로 변경한 후, 콘솔에 출력합니다. 이 예제에서 문자열은 본래 리스트이기 때문에 lazy하게 동작하고, `getContents`는 위에서도 언급한 것처럼 lazy I/O입니다. 또한 한번에 전체를 읽지않기 때문에 화면에 출력하기 전에 메모리에 저장하고, 정말 필요할때 입력으로부터 한 라인씩만 읽어서 화면에 출력합니다. 

```haskell
**[terminal]
**[prompt $ ]**[command cat haiku.txt | ./capslocker]
I'M A LIL' TEAPOT  
WHAT'S WITH THAT AIRPLANE FOOD, HUH?  
IT'S SO SMALL, TASTELESS  
```

`capslocker` 프로그램을 직접 실행하면 아래와 같이 라인단위로 입력하고 출력할 수 있습니다. 

```haskell
**[terminal]
**[prompt $ ]**[command ./capslocker]
hey ho  
HEY HO  
lets go  
LETS GO  
```

`capslocker`프로그램은 `getContents`에서 `contents`에 바인딩하고 `toUpper`로 변환할때까지 값에 대한 평가가 이루어 지지 않습니다. `putStr`를 수행했을때 비로소 이전에 수행하기로 했던 동작들을 수행하고 문자열을 만들어 출력합니다. 그리고 EOF를 만날때까지 계속해서 `putStr`은 종료되지 않고 이 동작들을 반복하게 됩니다. 

### interact

이번에는 어떤 입력을 받아서 한 라인이 10개 이하의 문자일때만 출력하는 프로그램을 만들어 보겠습니다. 

```haskell
main = do  
    contents <- getContents  
    putStr (shortLinesOnly contents)  
  
shortLinesOnly :: String -> String  
shortLinesOnly input =   
    let allLines = lines input  
        shortLines = filter (\line -> length line < 10) allLines  
        result = unlines shortLines  
    in  result
```

이 프로그램에서는 `shortLinesOnly`로 문자열을 처리하는 부분을 묶어서 I/O가 발생하는 부분을 최소화하였습니다. `shortLinesOnly` 함수는 문자열을 입력으로 받아서 문자열을 리턴하는 함수입니다. `shortLinesOnly`은 아래와 같이 동작합니다. 

1. `"short\nlooooooooooooooong\nshort again"`와 같은 문자열을 입력 받음
2. 입력 문자열에 `lines` 함수를 적용 -> `["short", "looooooooooooooong", "short again"]`
3. 리스트의 각 문자열을 길이가 10이하인 것으로 필터링 -> `["short", "short again"]`
4. `unlines`를 수행하여 하나의 새로운 라인으로 병합 -> `"short\nshort again"`

아래 다른 입력으로 실행해본 예제입니다. 

```
i'm short  
so am i  
i am a loooooooooong line!!!  
yeah i'm long so what hahahaha!!!!!!  
short line  
loooooooooooooooooooooooooooong  
short  
```

```haskell
**[terminal]
**[prompt $ ]**[command ghc -make shortlinesonly]
[1 of 1] Compiling Main             ( shortlinesonly.hs, shortlinesonly.o )  
Linking shortlinesonly ... 
**[prompt $ ]**[command  cat shortlines.txt | ./shortlinesonly]
i'm short  
so am i  
short
```

위 프로그램과 같이 입력으로부터 어떤 문자열을 읽어서 함수로 변환한 다음 출력하는 패턴은 `interact` 함수를 사용하여 더 간단히 할 수 있습니다. `interact` 함수는 `String -> String` 형태의 함수를 매개변수로 받고, 어떤 입력을 받아서 함수의 결과를 출력하는 I/O 작업을 반환합니다. 위에서 작성한 함수를 `interact`를 사용해서 재작성하면 아래와 같습니다. 

```haskell
main = interact shortLinesOnly  
  
shortLinesOnly :: String -> String  
shortLinesOnly input =   
    let allLines = lines input  
        shortLines = filter (\line -> length line < 10) allLines  
        result = unlines shortLines  
    in  result
```

이 예제에서는 코드는 약간 더 간결하고 읽기쉬워졌습니다. 함수 합성을 하면 아래와 같이 더 간결하게 작성할 수 있습니다. 

```haskell
main = interact $ unlines . filter ((<10) . length) . lines
```

이번 예제에서는 코드가 한라인으로 간결해진 것을 확인할 수 있습니다. 

`interact`는 예를들어 아래와 같은 프로그램을 만들때 사용됩니다. 

- 어떤 컨텐츠를 파이프에 담고 결과를 덤프하는 프로그램
- 사용자 입력의 한라인을 받아서, 그 라인을 기반으로 어떤 결과를 돌려주고 다른 라인을 받는 프로그램

이번에는 계속해서 한라인을 읽고, 라인이 palindrome(앞으로 읽으나 뒤로 읽으나 같은 문장)인지 알려주는 프로그램을 만들어보겠습니다. 간단하게 `getLine` 함수로 한라인을 읽어와서 palindrome인지 확인하여 알려주고, 다시 `main`
을 수행하면 됩니다. `interact`를 사용하면 더 간단해 집니다. 입력을 어떻게하면 원하는 출력으로 바꿀 수 있을지만 고민하면 됩니다. 따라서 각 입력 라인을 확인하여 `"palindrome"` 또는 `" not a palindrome"` 문자열로 변환하는 함수를 만들어 보겠습니다. 


```haskell
respondPalindromes contents = unlines (map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") (lines contents))  
    where   isPalindrome xs = xs == reverse xs
```

이것을 point-free로 다시 작성하면 아래와 같습니다. 

```haskell
respondPalindromes = unlines . map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") . lines  
    where   isPalindrome xs = xs == reverse xs
```

이 함수에 `"elephant\nABCBA\nwhatever"`를 넣어보면 아래와 같이 동작할 것입니다. 

1. 입력 문자열을 라인 단위의 리스트로 변화 -> `["elephant", "ABCBA", "whatever"]`
2. 각 라인 문자열을 람다함수에 넣어서 변환 -> `["not a palindrome", "palindrome", "not a palindrome"]`
3. `unlines`를 수행하여 한라인으로 병합 -> `"not a palindrome\npalindrome\nnot a palindrome"`

이제 이렇게 만들어진 함수를 `interact` 함수에 적용하기만 하면 됩니다. 

```haskell
main = interact respondPalindromes
```

```haskell
**[terminal]
**[prompt $ ]**[command runhaskell palindromes.hs]
hehe  
not a palindrome  
ABCBA  
palindrome  
cookie  
not a palindrome
```




























 






































