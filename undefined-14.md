# 랜덤 데이터

랜덤 데이터는 프로그래밍할때 자주 필요합니다. 이번 절에서는 하스켈에서 랜덤 데이터를 생성하는 방법을 알아보겠습니다.  
대부분의 언어들은 랜덤 숫자를 돌려주는 함수를 제공합니다. 이 함수들은 호출할때마다 다른 랜덤 숫자들을 반환합니다. 그러나 하스켈은 순수한 함수형 언어이고, 이것은 참조 투명성\(referential transparency\)을 보장한다는 의미입니다. 즉 동일한 입력이 두번 주어졌을때 항상 동일한 결과가 두번 생성되어야 함을 의미합니다. 참조 투명성은 프로그램에 대한 추론을 가능하게하고\(예측 가능하게하고\), 필요한 순간까지 평가를 늦출 수 있게합니다. 함수를 호출하면 결과가 반환되기전에 다른 변화가 일어나지 않을 것을 확신할 수 있습니다. 그러나 이러한 특징때문에 랜덤 데이터를 얻기는 까다롭습니다.

```haskell
randomNumber :: (Num a) => a  
randomNumber = 4
```

이 함수를 항상 4를 반환하기 때문에 난수 함수로서 사용할 수 없습니다.

다른 언어의 경우, 컴퓨터로부터 현재 시간, 마우스를 움직인 정도, 컴퓨터의 소음 등과같은 다양한 정보를 얻어서 난수를 생성합니다. 이러한 정보들의 조합은 시간마다 다를 수 있으므로 난수라고 할 수 있습니다.

하스켈에서는 임의성\(randomness\)을 매개변수로 사용하고 어떤 숫자\(또는 다른 데이터 타입\)를 반환하는 함수를 만들어서 랜덤 숫자를 만들수 있습니다.

`System.Random` 모듈은 임의성을 만족시키는 모든 함수들을 포함하고 있습니다. 그중하나인 `random` 함수에 대해서 살펴보겠습니다. 함수의 타입은 `random :: (RandomGen g, Random a) => g -> (a, g)` 입니다. 여기서 몇가지 새로운 타입클래스가 등장했습니다. `RandomGen` 타입클래스는 임의성의 원천 역할을 할 수 있는 타입들을 위한 것 입니다. `Random` 타입클래스는 랜덤 값을 얻을 수 있는 타입들입니다. 예를들어 boolean 값은 `True` 또는 `False`을 랜덤값으로 얻을 수 있습니다. 숫자의 경우는 많은 랜덤값들을 얻을 수 있습니다. `random` 함수의 타입을 풀어서 설명하면, 랜덤 생성기를 받아서 랜덤값과 새로운 랜덤 생성기를 반환하는 함수입니다. 여기서 왜 새로운 랜덤 생성기를 반환할까요?

`random` 함수를 사용하기 위해서는 랜덤 생성기가 필요합니다. `System.Random` 모듈에는 `StdGen`이라는 `RandomGen` 타입클래스의 인스턴스인 타입이 있습니다. `StdGen`은 수동으로 만들거나 시스템으로부터 받을 수 있습니다.

수동을 랜덤 생성기를 만드려면 `mkStdGen` 함수를 사용합니다. 함수 타입은 `mkStdGen :: Int -> StdGen` 입니다. Integer값 하나를 받아서 랜덤 생성기를 반환합니다. `random`과 `mkStdGen`를 사용해서 랜덤 숫자를 얻어보겠습니다.

```haskell
ghci> random (mkStdGen 100)
<interactive>:1:0:  
    Ambiguous type variable `a' in the constraint:  
      `Random a' arising from a use of `random' at <interactive>:1:0-20  
    Probable fix: add a type signature that fixes these type variable(s)
```

`random` 함수는 `Random` 타입클래스의 일부인 어떤 타입의 값을 반환할 수 있으므로, 타입을 명시해주어야 합니다. 따라서 아래와같이 실행되어야 합니다.

```haskell
ghci> random (mkStdGen 100) :: (Int, StdGen)
(-1352021624,651872571 1655838864)
```

튜플의 첫번째 값은 숫자이고, 두번째 값은 새로운 랜덤 생성기의 문자열 표현입니다. 여기서 동일한 랜덤 생성기를 사용하여 다시 `random`을 호출하면 어떻게 될까?

```haskell
ghci> random (mkStdGen 100) :: (Int, StdGen)
(-1352021624,651872571 1655838864)
```

동일한 파라메터로 동일한 함수를 호출했으므로 동일한 결과가 나왔다. 이번에는 다른 랜덤 생성기를 파라메터로 실행해보자.

```haskell
ghci> random (mkStdGen 949494) :: (Int, StdGen)
(539963926,466647808 1655838864)
```

이번에는 다른 숫자가 나왔다. 타입 어노테이션을 바꾸면 다른 타입의 값을 얻을 수 있다.

```haskell
ghci> random (mkStdGen 949488) :: (Float, StdGen)
(0.8938442,1597344447 1655838864)
ghci> random (mkStdGen 949488) :: (Bool, StdGen)
(False,1485632275 40692)
ghci> random (mkStdGen 949488) :: (Integer, StdGen)
(1691547873,1597344447 1655838864)
```

동전 3번 던지기를 시뮬레이션하는 함수를 만들어 보겠습니다. 만약 `random` 함수가 새로운 랜덤 생성기를 같이 반환하지 않는다면, 파라메터로 세개의 랜덤 생성기를 받는 함수를 만들어서 동전을 던져야 합니다. 만약 하나의 생성기가 한개의 `Int`형 랜덤값을 만들수 있다면, 3개의 동전 던지기를 만들수 있어야 합니다. 즉, 정확히 8개의 조합을 얻을 수 있습니다. 그러나 한번에 3개의 랜덤 생성기를 넘겨서 실행하면 3개의 결과만 얻을 수 있습니다. 그래서 `random` 함수가 반환한 새로운 랜덤 생성를 활용해야 합니다.

동전을 `Bool` 타입을 표현하고, 앞면은 `True`, 뒷면은 `False`로 하고, 함수를 만들어보겠습니다.

```haskell
threeCoins :: StdGen -> (Bool, Bool, Bool)  
threeCoins gen =   
    let (firstCoin, newGen) = random gen  
        (secondCoin, newGen') = random newGen  
        (thirdCoin, newGen'') = random newGen'  
    in  (firstCoin, secondCoin, thirdCoin)
```

`random` 함수를 호출하여 받은 새로운 랜덤 생성기를 다시 호출할때 파라메터로 사용하였습니다. 만약 계속 동일한 랜덤 생성기로 호출했다면 세개의 코인은 모두 같은 값을 반환해서 `(False, False, False)` 또는 `(True, True, True)`를 반환했을 것 입니다.

```haskell
ghci> threeCoins (mkStdGen 21)
(True,True,True)
ghci> threeCoins (mkStdGen 22)
(True,False,True)
ghci> threeCoins (mkStdGen 943)
(True,False,True)
ghci> threeCoins (mkStdGen 944)
(True,True,True)
```

위 예제에서는 이미 선언할때 `Bool`을 명시하였기 때문에, `random gen :: (Bool, StdGen)`와같이 어노테이션을 붙일 필요는 없다.

4개 또는 5개의 동전을 뒤짚으려면 어떻게 해야할까요? 이때는 `randoms` 함수를 사용합니다. 이 함수는 하나의 랜덤 생성기를 받아서 값들을 계속해서 생성합니다.\(무한대\)

```haskell
ghci> take 5 $ randoms (mkStdGen 11) :: [Int]
[-1807975507,545074951,-1015194702,-1622477312,-502893664]
ghci> take 5 $ randoms (mkStdGen 11) :: [Bool]
[True,True,True,True,False]
ghci> take 5 $ randoms (mkStdGen 11) :: [Float]
[7.904789e-2,0.62691015,0.26363158,0.12223756,0.38291094]
```

`randoms` 함수는 왜 새로운 랜덤 생성기를 반환하지 않을까요? `randoms` 함수를 직접 구현해본다면 아래와 같습니다.

```haskell
randoms' :: (RandomGen g, Random a) => g -> [a]  
randoms' gen = let (value, newGen) = random gen in value:randoms' newGen
```

재귀를 사용하였습니다. 현재 생성기로부터 랜덤값과 새로운 생성기를 받고, 랜덤값으로 head로 하는 리스트를 만듭니다. 그리고 리스트의 나머지는 새로운 랜덤 생성기를 파라메터로 다시 `randoms`를 호출하여 만듭니다.

다음 예제와 같이 유한한 숫자들의 스트림과 새로운 랜덤 생성기를 생성하는 함수를 만들 수 있습니다.

```haskell
finiteRandoms :: (RandomGen g, Random a, Num n) => n -> g -> ([a], g)  
finiteRandoms 0 gen = ([], gen)  
finiteRandoms n gen =   
    let (value, newGen) = random gen  
        (restOfList, finalGen) = finiteRandoms (n-1) newGen  
    in  (value:restOfList, finalGen)
```

`finiteRandoms` 함수도 재귀를 사용하였습니다. 이 함수는 입력으로 생성할 랜덤값의 개수를 받습니다. 만약 0을 받으면 빈리스트와 동일한 생성기를 반환하여 재귀를 종료합니다. 먼저 `random`을 호출하여 하나의 랜덤값과 새로운 랜덤 생성기를 얻습니다. 랜덤값을 리스트의 head로 합니다. 나머지 랜덤값을 만들기 위해서 `finiteRandoms`를 재귀호출합니다. 이때 파라메터로 n-1과 새로운 랜덤 생성기를 넣습니다. 최종적으로 n개의 랜덤값을 포함한 리스트와 마지막 랜덤 생성기를 반환합니다.

지정된 범위내에서 랜덤값을 생성하려면 어떻게 할까요? 이때는 `randomR` 함수를 사용합니다. 이 함수의 타입은 `randomR :: (RandomGen g, Random a) :: (a, a) -> g -> (a, g)` 입니다. `random` 함수와의 차이점은 입력 파라메터로 값의 범위를 지정할 수 있는 튜플을 받는다는 것 입니다.

```haskell
ghci> randomR (1,6) (mkStdGen 359353)
(6,1494289578 40692)
ghci> randomR (1,6) (mkStdGen 35935335)
(3,1250031057 40692)
```

랜덤값이 입력받은 범위내에 있는 것을 확인할 수 있습니다.

`randomRs` 함수는 지정된 범위내의 랜덤값들의 스트림을 만듭니다.

```haskell
ghci> take 10 $ randomRs ('a','z') (mkStdGen 3) :: [Char]
"ndkxbvmomg"
```

지금까지는 어떤 임의의 정수를 사용하여 수동으로 랜덤값을 생성하였습니다. 실제 프로그램에서 이렇게하면 항상 같은 랜덤값을 반환하게 됩니다. 그래서 `System.Random` 모듈에서는 `IO StdGen` 타입인 `getStdGen` I/O 작업을 제공합니다. 이것은 프로그램이 시작될때, 시스템에 좋은 랜덤 생성기가 있는지 묻고 저장합니다. 이것은 전역 생성기라고 부릅니다. `getStdGen`은 전역 랜덤 생성기를 어딘가에 바인딩할때 가져옵니다.

`getStdGen`를 사용하여 랜덤 문자열을 생성하는 프로그램을 만들어보겠습니다.

```haskell
import System.Random  

main = do  
    gen <- getStdGen  
    putStr $ take 20 (randomRs ('a','z') gen)
```

```haskell
$ runhaskell random_string.hs
pybphhzzhuepknbykxhe
$ runhaskell random_string.hs
eiqgcxykivpudlsvvjpg
$ runhaskell random_string.hs
nzdceoconysdgcyqjruo
$ runhaskell random_string.hs
bakzhnnuzrkgvesqplrx
```

여기서 `getStdGen`을 두번 실행하면 시스템에 동일한 전역 랜덤 생성기가 두번 요청되는 것을 주의해야 합니다. 예를들어 아래와 같이 작성하면,

```haskell
import System.Random  

main = do  
    gen <- getStdGen  
    putStrLn $ take 20 (randomRs ('a','z') gen)  
    gen2 <- getStdGen  
    putStr $ take 20 (randomRs ('a','z') gen2)
```

동일한 문자열을 두번 출력합니다. 서로다른 두개의 길이 20의 문자열을 얻는 한가지 방법은 무한 스트림으로 받아서 20개 문자씩 잘라서 출력하는 것입니다. 문자열을 원하는 길이로 자르기 위해서 `Data.List`에 정의된 `splitAt`을 사용합니다. `splitAt` 함수는 입력받은 인덱스를 기준으로 앞부분과 뒷부분에 대한 튜플을 반환합니다.

```haskell
import System.Random  
import Data.List  

main = do  
    gen <- getStdGen  
    let randomChars = randomRs ('a','z') gen  
        (first20, rest) = splitAt 20 randomChars  
        (second20, _) = splitAt 20 rest  
    putStrLn first20  
    putStr second20
```

또다른 방법으로는 `newStdGen`을 사용하는 방법입니다. 이 함수는 랜덤 생성기를 두개의 생성기로 나누고, 그 중하나의 전역 랜덤 생성기를 업데이트 합니다. 그리고 나머지 하나의 결과로 캡슐화합니다.

```haskell
import System.Random  

main = do     
    gen <- getStdGen     
    putStrLn $ take 20 (randomRs ('a','z') gen)     
    gen' <- newStdGen  
    putStr $ take 20 (randomRs ('a','z') gen')
```

`newStdGen`은 어떤것에 바인딩할때 새로운 랜덤 생성기를 얻을뿐아니라 전역 랜덤 생성기를 갱신하기 때문에, 다시 `getStdGen`을 호출하면 처음에 받은 `gen`과는 다른 생성기를 얻을 수 있습니다.

아래 예제는 사용자가 어떤 숫자를 생각하고 있는지 추측하는 프로그램입니다.

```haskell
import System.Random  
import Control.Monad(when)  

main = do  
    gen <- getStdGen  
    askForNumber gen  

askForNumber :: StdGen -> IO ()  
askForNumber gen = do  
    let (randNumber, newGen) = randomR (1,10) gen :: (Int, StdGen)  
    putStr "Which number in the range from 1 to 10 am I thinking of? "  
    numberString <- getLine  
    when (not $ null numberString) $ do  
        let number = read numberString  
        if randNumber == number   
            then putStrLn "You are correct!"  
            else putStrLn $ "Sorry, it was " ++ show randNumber  
        askForNumber newGen
```

`askForNumber` 함수는 랜덤 숫자 생성기를 받아서 사용자에게 랜덤 숫자를 보여주고 맞는지 확인하는 I/O 작업을 반환합니다. 함수 내부에서는 먼저 입력받은 생성기를 기반으로 랜덤 숫자와 새로운 생성기를 받아서 `randNumber`와 `newGen`에 할당하였습니다. 만약 `7`이라는 숫자를 생성했다고 가정하면, 어떤 숫자가 생성되었을지 1 ~ 10 사이에 아무 숫자나 추측해서 입력하고 합니다. 입력받은 숫자를 `numberString`에 담고, `when`에서 입력값 체크를 수행하였습니다. 만약 잘못된 입력이 들어오면 `main` 함수의 끝을마나서 빈 I/O 작업인 `return ()`가 수행됩니다. 정상 입력인 경우, 랜덤 숫자와 비교해서 맞았는지 여부를 알려줍니다. 그리고나서 `askForNumber`를 재귀적으로 호출합니다.

> 만약 "haha"와 같은 `read` 함수로 읽을 수 없는 입력이 들어오면 이상한 에러 메시지와 함께 프로그램이 죽을것입니다. 만약 이런 잘못된 입력에 죽지않게 하려면 문자열을 읽는 작업이 실패하면 빈리스트를 반환하는 `reads`를 사용합니다. 읽기작업에 성공하면 읽어드린 문자열을 하나의 구성요소로 포함한 튜플을 가진 싱글톤 리스트를 반환합니다.

프로그램을 실행하면 아래와같이 동작합니다.

```haskell
$ runhaskell guess_the_number.hs
Which number in the range from 1 to 10 am I thinking of? 4  
Sorry, it was 3  
Which number in the range from 1 to 10 am I thinking of? 10  
You are correct!  
Which number in the range from 1 to 10 am I thinking of? 2  
Sorry, it was 4  
Which number in the range from 1 to 10 am I thinking of? 5  
Sorry, it was 10  
Which number in the range from 1 to 10 am I thinking of?
```

