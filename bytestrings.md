## Bytestrings

리스트는 매우 유용하고 가장 많이쓰이는 자료구조입니다. 하스켈에서는 특히 리스트가 다른 언어의 while이나 for문 같은 루프를 대체할 수 있습니다. 이게 가능한 이유는 하스켈의 게으른 특성때문에 필요한 시점에 딱 한번에 평가되기 때문입니다. 이런 특성은 무한 리스트나 무한 리스트의 무한 리스트에서도 문제없이 필터링과 맵핑과같은 동작을 가능케합니다. 그래서 표준 입력이나 파일을 읽을때 리스트를 사용하여 스트림을 표현할 수 있습니다. 실제로 필요할때만 파일을 액세스해서 열거나 문자열로 읽을 수 있습니다. 

하지만 파일을 문자열로 처리하는 것은 느리다는 단점이 있습니다. `String`은 `[Char]`와 같습니다. `Char`는 유니코드에서 문자를 나타내기 위해서 몇바이트가 사용되기 때문에 고정된 크기를 가지고 있지않습니다. 또한 리스트들은 게으르게 평가됩니다. 만약 `[1,2,3,4]`라는 리스트가 있다면, 정말로 값이 필요하기 전까지는 평가되지않고 promise 상태로 있을 것 입니다. 만약 출력과 같은 것을 통해서 리스트의 첫번째 요소를 강제로 평가시켜도, 리스트의 나머지 요소들은 여전히 평가되지 않은 상태입니다. 간단한 리스트를 처리하는데 게으르게 평가되도록 하는 것이 항상 효율적이진 않습니다. 

보통은 게으른 평가로 인한 오버헤드는 무시할 수준이지만, 큰 사이즈의 파일을 읽을때는 다룰때는 한번에 평가하는 것이 큰 부담이 됩니다. 그래서 하스켈은 **bytestrings**를 제공합니다. Bytestrings는 각 요소가 1바이트(8bits)인 리스트입니다. 또한 laziness에 대해서도 다르게 동작합니다. 

Bytestrings는 strict와 lazy, 두가지 종류가 있습니다. `Data.ByteString`에는 strict bytestrings이 있고 laziness와는 완전히 반대의 특성을 가집니다. strict는 promise가 없고, 바이트들의 배열입니다. 또한 무한한 strict bytestrings는 가질 수 없습니다. strict bytestrings의 첫번째 바이트가 평가되면, 전체가 다 평가됩니다. strict bytestrings의 장점은 promise가 없기때문에 오버헤드가 적다는 것 입니다. 단점은 즉시 메모리에서 읽기때문에 메모리 효율이 떨어집니다. 

`Data.ByteString.Lazy`에는 lazy bytestrings가 있습니다. 이것은 게으른 특성을 지니지만, 리스트만큼 게으르진 않습니다. 리스트의 구성요소만큼 많은 thunk를 가지고 있습니다. Lazy bytestrings은 64K 크기로 묶어서 저장해 놓습니다. 따라서 만약 lazy bytestrings을 출력과 같은 것을 통해서 평가하면 첫 64K가 평가됩니다. 그리고 나머지 묶음은 promise가 됩니다. Lazy bytestrings은 64K 크기의 strict bytestrings의 리스트와 같습니다. 따라서 파일의 값을 평가할때 이 묶음 단위로 읽어드립니다. 이러게하면 메모리 오버헤드가 급격하게 상승하지않고 CPU의 L2 캐시에 64K 크기가 딱 맞기때문에 매우 빠르게 읽을수 있습니다.

`Data.ByteString.Lazy`안에는 `Data.List`의 함수와 거의 동일한 함수들을 대부분 제공합니다. 함수들의 타입이 `[a]` 대신 `ByteString`을, `a` 대신 `Word8`을 사용한 것만 다릅니다. 또한 함수의 동작들도 거의 유사합니다. 지금부터 사용하는 예제는 아래와 같은 qualified import를 사용할 것 입니다. 

```haskell
import qualified Data.ByteString.Lazy as B  
import qualified Data.ByteString as S
```

`B`는 lazy bytestrings이고, `S`는 strict bytestrings 입니다. 대부분은 lazy 버전이 사용될 것 입니다. 

**`pack`** 함수의 타입은 `pack :: [Word8] -> ByteString` 입니다. 즉, `Word8`의 리스트를 받아서 `ByteString`을 반환합니다. 리스트를 받는다는 것만으로도 게으르게 동작할 것이라는 것을 예측할 수 있습니다. 그리고 조금 덜 게으르게 하기 위해서 64K 간격으로 게으르게 동작할 것 입니다. 

여기서 `Word8`은 `Int`와 비슷합니다. 단지 0-255 범위의 8-bit 숫자만 표현됩니다. `Int`처럼 `Num` 타입클래스에 포함됩니다. 예를들어 `5`는 숫자 타입처럼 동작한다는 점에서 다형성을 가지고, `Word8` 타입으로도 사용될 수 있습니다.  

```haskell
**[terminal]
**[prompt ghci> ]**[command B.pack [99,97,110]]
Chunk "can" Empty
**[prompt ghci> ]**[command B.pack [98..120]]
Chunk "bcdefghijklmnopqrstuvwx" Empty
```

실제로 사용할때 `Word8`에 대해서 신경쓸 필요는 없습니다. 위와같이 타입시스템이 알아서 선택할 것 입니다. 만약 `336`와 같은 `Word8`의 범위를 벗어나는 값을 시도하면 약 `80`으로 취급합니다. 

`pack` 함수를 사용해서 간단하게 `ByteString`안에 값을 넣었습니다. 이것은 하나의 묶음(Chunk)안에 들어갑니다. `Empty`는 리스트의 `[]`같은 것 입니다. 

**`unpack`** 함수는 `pack` 함수의 반대입니다. bytestrings을 받아서 바이트의 리스트로 바꾸어 줍니다. 

**`fromChunks`** 함수는 strict bytestrings의 리스트를 받아서 lazy bytestrings로 변환해줍니다. **`toChunks`** 함수는 lazy bytestrings를 받아서 strict bytestrings의 리스트로 바꾸어줍니다. 

```haskell
**[terminal]
**[prompt ghci> ]**[command B.fromChunks [S.pack [40,41,42], S.pack [43,44,45], S.pack [46,47,48]]]
Chunk "()*" (Chunk "+,-" (Chunk "./0" Empty))
```

이 함수는 많은 작은 strict bytestrings가 있을때 유용합니다. 메모리에서 한개의 큰 string bytestrings안에서 조합하지않고 효율적으로 처리할 수 있습니다. 

`:`의 bytestrings 버전으로 **`cons`** 함수가 있습니다. `cons`는 한개의 바이트를 받아서 bytestring의 첫번째 바이트에 넣어줍니다. 이 함수가 동작할때는 게으르게 동작해서 chunk가 가득차있지 않아도 내부적으로 새로운 chunk를 만들 것 입니다. 따라서 좀 더 효율적으로 하려면 strict 버전인 `cons'`를 사용합니다. 만약 많은 양의 바이트들을 bytestring의 첫번째 바이트를 추가해야한다면 strict 버전을 사용하는게 좋습니다. 

```haskell
**[terminal]
**[prompt ghci> ]**[command B.cons 85 $ B.pack [80,81,82,84]]
Chunk "U" (Chunk "PQRT" Empty)
**[prompt ghci> ]**[command B.cons' 85 $ B.pack [80,81,82,84]]
Chunk "UPQRT" Empty
**[prompt ghci> ]**[command foldr B.cons B.empty [50..60]]
Chunk "2" (Chunk "3" (Chunk "4" (Chunk "5" (Chunk "6" (Chunk "7" (Chunk "8" (Chunk "9" (Chunk ":" (Chunk ";" (Chunk "<"  
Empty))))))))))
**[prompt ghci> ]**[command foldr B.cons' B.empty [50..60]]
Chunk "23456789:;<" Empty
```

**`empty`** 함수는 빈 bytestring을 만듭니다. 위 예제에서 `cons`와 `cons'` 동작의 차이점을 확인하시기 바랍니다. `foldr`을 사용해서 빈 bytestring에서 시작해서 오른쪽에서부터 입력된 바이트 리스트를 하나씩 빼서 넣었습니다. `cons`를 사용했을때는 모든 바이트가 각각 하나의 chunk안에 들어갔습니다. 이 경우, `cons'`를 사용하는 것이 효율적입니다.

여기서 다루지않은 `Data.List`에도 존재하는 `head`, `tail`, `init`, `null`, `length`, `map`, `reverse`, `foldl`, `foldr`, `concat`, `takeWhile`, `filter` 등의 함수도 존재합니다. 

또한 `System.IO` 모듈에 있는 몇몇 함수도 `String`이 `ByteString`으로만 바뀐 동일한 이름의 함수들이 있습니다. 예를들어 `System.IO`에서는 `readFile` 함수의 타입이 `readFile :: FilePath -> IO String`이지만 bytestring 모듈에서 **`readFile`** 함수의 타입은 `readFile :: FilePath -> IO ByteString`입니다.  `readFile` 함수를 사용할때는 strict bytestrings를 사용하면 파일을 읽을때 메모리안에서 한번에 읽는다는점에 유의해야 합니다. lazy bytestrings로 읽으면 묶음(chunk) 단위로 읽혀집니다. 

커맨드라인 인자로 두개의 파일명을 받아서 첫번째 파일을 두번째 파일에 복사하는 프로그램을 만들어보겠습니다. 
`System.Directory` 모듈안에는 이미 `copyFile` 함수가 있지만, 직접 간단한 파일 복사 프로그램을 구현해보는 것 입니다.

```haskell
import System.Environment  
import qualified Data.ByteString.Lazy as B  
  
main = do  
    (fileName1:fileName2:_) <- getArgs  
    copyFile fileName1 fileName2  
  
copyFile :: FilePath -> FilePath -> IO ()  
copyFile source dest = do  
    contents <- B.readFile source  
    B.writeFile dest contents
```

여기서는 두개의 `FilePath`를 받아서 bytestrings을 사용해서 하나의 파일을 다른 파일에 복사하는 I/O 작업을 반환하는 함수 `copyFile`을 만들었습니다. `main` 함수에서는 매개변수를 받아서 `copyFile`를 호출하는 역할만 하고 있습니다. 실행해보면 아래와 같습니다. 

```haskell
**[terminal]
**[prompt $ ]**[command runhaskell bytestringcopy.hs something.txt
```

언뜻보면 bytestring를 사용하지않는 프로그램처럼 보일 수 있습니다. 하지만 실제로는 `B.readFile`, `B.writeFile`을 사용하였습니다. 대부분은 간단히 qualified import로 모듈만 변경하는 것으로 `String`을 사용하는 프로그램을 `ByteString`을 사용하는 프로그램으로 바꿀 수 있습니다. 문자열에 많은 양의 데이터를 읽는 프로그램의 성능을 향상시킬때, bytestrings을 적용해보는 것으로도 성능을 향상시킬 수 있습니다.