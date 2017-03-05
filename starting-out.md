# 기본 연산자

하스켈도 스크립트 언어처럼 실행해볼 수 있는 REPL을 지원합니다. 터미널이나 [하스켈 공식 웹 사이트](https://www.haskell.org/)를 통해서 간단히 설치가 가능합니다.

![](https://cdn-images-1.medium.com/max/1600/1*xx4YIGBJaqGnPrA3MVm6pA.png)

설치 후, ghci라고 입력하시면 REPL안으로 진입합니다.

![](https://cdn-images-1.medium.com/max/1600/1*rOfwhbl07OIOKpdOioVxpw.png)

위와 같은 명령어로 간단히 프롬프트를 바꿀 수 있습니다.

![](https://cdn-images-1.medium.com/max/1600/1*yiRHKrbWxJtfyrQApK0qcQ.png)

위와 같이 더하기, 곱하기, 빼기, 나누기 연산을 지원하고, 기본적으로 앞에서 부터 실행됩니다. 연산자 우선순위 조정은 괄호를 사용해서 변경할 수 있습니다. \(수학과 비슷하지요\)

![](https://cdn-images-1.medium.com/max/1600/1*4QNNolwRUo29aPpvGkcw-Q.png)

여기서 음수를 사용할때는 약간 주의할 부분이 있습니다.

![](https://cdn-images-1.medium.com/max/1600/1*aWBE3ZP2obSmt-7KiHri6Q.png)

위와 같이 음수를 그냥 쓰면 에러가 나옵니다. 음수를 사용할 때는 항상 괄호와 함께 사용하는 것이 좋습니다.

![](https://cdn-images-1.medium.com/max/1600/1*CICWQHXxkXAGrKFeqM9vsw.png)

하스켈에서는 boolean값을 True, False로 표현하고, &&\(and\), \|\|\(or\), not\(java의 !와 같음\)를 사용할 수 있습니다.

![](https://cdn-images-1.medium.com/max/1600/1*i4xmlwxQZ39VsK5jcujnWA.png)

비교 연산자로 ==, /=\(Java의 !=과 같음\)를 사용하고 있습니다.

![](https://cdn-images-1.medium.com/max/1600/1*7nYl3xjd2rrZnRocK_tqXw.png)

하스켈에서는 +, ==과 같은 연산자도 함수 입니다. 이렇게 양쪽에 입력 파라메터를 받는 함수들을 infix function이라고 부릅니다. 이런 함수에서 양쪽에 다른 타입이 오면 위와 같은 에러가 발생합니다.

![](https://cdn-images-1.medium.com/max/1600/1*XV1kQ6PMPVUtCpc-jDDOeg.png)

위 명령에서 5는 integer 또는 float가 될 수 있습니다. 이 경우는 뒤에 4.0으로 float이 왔기 때문에 5는 float이 됩니다.

![](https://cdn-images-1.medium.com/max/1600/1*JfyMhebFZ_CMJwBRHWgPzw.png)

하스켈에서는 함수의 파라메터를 space로 구분합니다. 위의 경우, succ, min, max가 함수이고 각각의 함수 뒤에 오는 숫자들이 함수의 입력 파라메터 입니다. C언어의 succ\(8\), min\(9, 10\), max\(100, 101\)와 같다고 생각하시면 됩니다.

![](https://cdn-images-1.medium.com/max/1600/1*T3XzwjmQUbRXKDmYjFcF6Q.png)

이런 함수들 역시 괄호를 사용하여 실행 순서를 변경할 수 있습니다.

![](https://cdn-images-1.medium.com/max/1600/1*Pxwh1-PZDH3ISlFurV71xA.png)

div와 같이 두개의 입력 파라메터를 받는 함수는 infix function으로 사용할 수 있습니다. 본문에서는 div같은 경우, infix로 쓰는 것이 더 이해하기 편하다고 권장하고 있습니다.

![](https://cdn-images-1.medium.com/max/1600/1*QsRfdEgG1dM4NDEr1BGAvQ.png)

하스켈에서 위와 같이 함수가 중첩으로 호출되는 경우는 C언어의 succ\(succ\(4\)\)와 같습니다.



