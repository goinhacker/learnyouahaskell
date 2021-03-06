# 타입동의어

## Type synonyms \(타입 동의어\)

이전 챕터에서 타입을 작성할때 `[Char]`와 `String`은 동일하고 서로 교체 가능하다고 배운바 있습니다. 이는 **타입 동의어**로 구현됩니다. 타입 동의어는 본질적으로 아무것도하지 않습니다. 단지 누군가가 우리의 코드나 문서를 읽을때 의미있는 다른 이름으로 타입을 주는 것입니다. 아래는 `[Char]`와 `String`을 동의어로 정의한 것 입니다.

```haskell
type String = [Char]
```

여기서 _type_ 키워드는 실제로 새로운 어떤것도 만들지 않았지만\(_data_ 키워드 처럼..\), 이미 존재하는 타입의 동의어를 만들었습니다.

만약 문자열을 대문자로 바꾸는 `toUpperString` 함수를 만든다면, `toUpperString :: [Char] -> [Char]` 또는 `toUpperString :: String -> String`와 같이 선언할 수 있습니다. 둘다 완전히 같지만, 후자가 더 읽기 좋습니다.

이전에 `Data.Map` 모듈을 다룰때, 맵안에서 변환하기 전에 연관 리스트\(사전\)로 전화번호부를 표현했습니다. 여기서 연관 리스트는 key-value 쌍의 리스트입니다. 이 전화번호부는 아래와 같습니다.

```haskell
phoneBook :: [(String,String)]  
phoneBook =      
    [("betty","555-2938")     
    ,("bonnie","452-2928")     
    ,("patsy","493-2928")     
    ,("lucille","205-2928")     
    ,("wendy","939-8282")     
    ,("penny","853-2492")     
    ]
```

여기서 `phoneBook`의 타입은 `[String, String]`입니다. 이것은 키와 값을 모두 문자열로 가지는 맵의 연관 리스트라는 것을 말해줍니다. 타입 선언안에 몇가지 추가정보를 전달하여 타입 동의어를 만들어 보겠습니다.

```haskell
type PhoneBook = [(String,String)]
```

이제는 전화번호부를 위한 타입 선언이 `phoneBook :: PhoneBook`가 될 수 있습니다. `String`에 대해서도 타입 동의어를 만들어 보겠습니다.

```haskell
type PhoneNumber = String  
type Name = String  
type PhoneBook = [(Name,PhoneNumber)]
```

`String` 타입 동의어는 하스켈 프로그래머가 함수에서 사용되는 문자열이 무엇을 나타내는지에 대한 추가 정보를 전달할때 사용합니다.

이렇게 하면 이름과 번호를 받는 함수를 구현할때, 좀 더 명확한 타입 선언을 할 수 있습니다.

```haskell
inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook
```

여기서 타입 동의어를 사용하지 않으면 타입선언은 `String -> String -> [(String, String)] -> Bool`가 됩니다. 이 경우, 타입 동의어를 사용하는 것이 더 이해하기 좋은 타입선언입니다. 하지만 지나치게 사용하면 안됩니다. 타입동의어는 함수안의 기존 타입을 좀 더 명확하게 하거나 `[String,String]`과 같이 반복되는 타입을 함수의 문맥에서 더 명확히 표현하기 위해서 사용합니다.

또한 타입 동의어도 매개변수화될 수 있습니다. 만약 모든 타입을 키와 값으로 사용할 수 있는 일반적인 연관 리스트이 필요하다면 아래와 같이 선언할 수 있습니다.

```haskell
type AssocList k v = [(k,v)]
```

`AssocList`를 사용하여 연관 리스트의 키로 값을 얻는 함수의 타입을 `(Eq k) => k -> AssocList k v -> Maybe v`와 같이 선언할 수 있습니다. 이 선언에서 `AssocList`는 `AssocList Int String`처럼, 두 타입을 받아서 하나의 구체적인 타입을 만드는 타입 생성자 입니다.

**Note**: 본 문서에서 말하는 구체적인 타입\(concrete type\)이란 `Map Int String`과 같이 완전한 타입이거나 `[a]`, `(Ord a) => Maybe a` 등과 같은 다형 함수들 중 하나를 의미합니다. 예를들어 `Maybe`는 타입이 아니고 타입 생성자이지만, `Maybe String`이 되면 이것은 구체적인 타입이라고 부릅니다. 모든 값들은 구체적인 타입을 가집니다.

새로운 함수를 받을때 부분 함수를 사용할 수 있는 것처럼, 새로운 타입 생성자를 얻을때 타입 매개변수를 부분 적용할 수 있습니다. **새로운 함수를 돌려받기 위해서 적은 매개변수를 가진 함수를 호출하는 것처럼, 적은 타입 매개변수를 가진 타입 생성자를 명시할 수 있고, 부분 적용된 타입 생성자를 돌려받을 수 있습니다.** 만약 `Data.Map`에서 `Int`를 다른 어떤 것으로 변환하는 타입을 표현한다면 아래와 같이 할 수 있습니다.

```haskell
type IntMap v = Map Int v
```

또는

```haskell
type IntMap = Map Int
```

어느 쪽이든, `IntMap` 타입 생성자는 하나의 매개변수를 받고, 이것은 `Int`가 변환될 타입입니다.

**Note:** `IntMap`을 실제로 구현하려면, `Data.Map`에서 qualified import를 사용해야 합니다. 이때 타입 생성자 또한 모듈 이름이 선행되어야 합니다. 따라서 `type IntMap = Map.Map Int`로 작성될 것 입니다.

타입 생성자와 값 생성자 사이의 차이를 명확히 구분할 수 있어야 합니다. 왜냐하면 `IntMap`이나 `AssocList`라는 타입 동의어를 만들었다고 해서 `AssocList [(1,2),(4,5),(7,9)]` 같은 것을 할 수 있는 것은 아닙니다. 타입 동의어가 의미하는 것은 다른 이름을 사용하여 타입을 참조할 수 있다는 점입니다. `[(1,2),(3,5),(8,9)] :: AssocList Int Int`와 같이 실행하여 키/값을 `Int`로 가정하여 숫자들의 연관 리스트로 만들 수 있지만, 여전히 내부에 정수 쌍이있는 일반 리스트로서만 사용할 수 있습니다.

**타입 동의어 및 일반적인 타입은 하스켈의 타입 위치에서만 사용**될 수 있습니다. 새로운 타입을 정의할때나\(_data_와 _type_ 선언에서\) `::` 뒷부분이 하스켈의 타입 위치입니다. `::`은 타입 선언 또는 타입 표시\(type annotation\)에 있습니다.

매개변수로 두개의 타입을 받는 데이터 타입으로 `Either a b`가 있습니다. 이것은 간략하게 아래와 같이 정의됩니다.

```haskell
data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)
```

여기에서는 두개의 값 생성자를 가집니다. 만약 `Left`가 사용되면 컨텐츠의 타입은 `a`이고, `Rigth`가 사용되면 컨텐츠의 타입은 `b` 입니다. 그래서 두개의 서로 다른 타입의 값을 캡슐화한 타입을 사용할 수 있습니다. 그리고 나서 타입 `Either a b`의 값을 얻을 수 있습니다. 일반적으로 `Left`와 `Right` 모두에서 패턴매칭하고, 그 중 어떤 것이었는지에 따라 다른 타입으로 사용합니다.

```haskell
ghci> Right 20
Right 20
ghci> Left "w00t"
Left "w00t"
ghci> :t Right 'a'
Right 'a' :: Either a Char
ghci> :t Left True
Left True :: Either Bool b
```

대부분의 경우, **어떤 연산의 결과가 실패하거나 실패하지 않는 것을 둘다 가질때** `Maybe a`**가 사용됩니다. 하지만 가끔** `Nothing`**은 많은 정보를 전달할 수 없기때문에** `Maybe a`**는 충분하지 않습니다. 함수가 어떻게 or 왜 실패했는지는 관심이 없고, 실패 여부만 필요할 때만 사용될 수 있습니다.** `Data.Map`는 맵안에 키가 없을때만 실패해서 어떤 일이 발생할지 우리가 정확히 알고 있습니다. **하지만 함수가 어떻게 왜 실패했는지에 관심이 있을때, 보통** `Either a b`**의 결과 타입을 사용합니다.** 여기서 `a`는 가능한 실패에 대한 것을 알려줄 수 있는 타입의 일종이고, `b`는 성공적인 연산의 타입입니다. 그러므로 에러는 `Left` 값생성자를 반대의 경우는 `Right`를 사용합니다.

예를들어 고등학교에 락커가 있고, 학생들은 포스터를 넣을 곳이 있습니다. 각 락커는 코드 조합을 가집니다. 학생이 새로운 락커가 필요할때, 락커 감독관에게 원하는 락커 번호를 알려주고 락커 코드를 받습니다. 하지만 만약 누가 이미 락커를 사용하고 있다면, 그는 락커의 코드를 알려줄 수 없고 다른 것을 골라야 합니다. 이 락커를 표현하기 위해서 `Data.Map`의 맵을 사용할 것 입니다. 락커 번호가 락커 사용 여부와 락커 코드의 쌍으로 매핑됩니다.

```haskell
import qualified Data.Map as Map  

data LockerState = Taken | Free deriving (Show, Eq)  

type Code = String  

type LockerMap = Map.Map Int (LockerState, Code)
```

간단하게 새로운 락커가 이미 사용중인지 나타내기 위한 _data_ 타입을 선언했습니다. 그리고 락커 코드를 위한 타입 동의어를 만들었습니다. 또한 정수를 락커 상태와 코드 쌍으로 변환하기 위한 타입 동의어를 만들었습니다. 이제부터는 락커 맵안에서 코드를 검색하는 함수를 만들 것입니다. 결과를 표현하기 위해서는 `Either String Code` 타입을 사용할 것 입니다. 왜냐하면 락커 번호가 존재하지 않거나 락커가 이미 사용 중일때 모두 검색 실패이기 때문입니다. 검색에 실패하면 `String`을 사용하여 무슨일이 발생했는지 알려줄 수 있습니다.

```haskell
lockerLookup :: Int -> LockerMap -> Either String Code  
lockerLookup lockerNumber map =   
    case Map.lookup lockerNumber map of   
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"  
        Just (state, code) -> if state /= Taken   
                                then Right code  
                                else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"
```

이 예제에서는 일반적인 맵 검색을 수행하였습니다. 만약 `Nothing`이면 `Left String` 타입의 값으로 락커가 존재하지 않는다는 메시지를 리턴합니다. 만약 검색되었다면 락커가 사용 중인지 확인합니다. 만약 사용 중이면 `Left`에 락커가 이미 사용 중이라는 메시지를 리턴합니다. 만약 아무도 사용하고 있지 않으면 `Right Code` 타입의 값으로 락커의 코드를 리턴합니다. 여기서 `Code`는 타입 동의어라서 실제로는 `Right String`을 리턴합니다. 아래는 맵 예제입니다.

```haskell
lockers :: LockerMap  
lockers = Map.fromList   
    [(100,(Taken,"ZD39I"))  
    ,(101,(Free,"JAH3I"))  
    ,(103,(Free,"IQSA9"))  
    ,(105,(Free,"QOTSA"))  
    ,(109,(Taken,"893JJ"))  
    ,(110,(Taken,"99292"))  
    ]
```

이제 실제로 사용해보면 아래와 같습니다.

```haskell
ghci> lockerLookup 101 lockers
Right "JAH3I"
ghci> lockerLookup 100 lockers
Left "Locker 100 is already taken!"
ghci> lockerLookup 102 lockers
Left "Locker number 102 doesn't exist!"
ghci> lockerLookup 110 lockers
Left "Locker 110 is already taken!"
ghci> lockerLookup 105 lockers
Right "QOTSA"
```

결과를 표현하기 위해서 `Maybe a`가 사용될 수도 있지만 이때는 왜 코드를 받을 수 없었는지 알 수 없습니다. 하지만 여기서는 결과 타입안에 실패에 대한 정보를 담았습니다.

