read.any <- function(text, sep = "", ...) {

  # readr 패키지에 있는 guess_encoding 함수를 사용하여
  # 문서의 인코딩을 추측합니다.
  # 다른 언어와 달리 한글과 같은 경우는 높은 확률로
  # 인코딩을 찾아주어 실사용에 큰 무리가 없습니다.
  encoding <- as.character(guess_encoding(text)[1,1])

  # 파일 확장자에 따라 구분점을 다르게 처리해주는 부분입니다.
  # 함수에 임의의 인자를 지정해 주는 경우를 제외하고는
  # csv 파일인 경우 "," 를 구분점으로 처리하고
  # txt 파일인 경우 "\n" 즉 엔터를 구분점으로 처리하였습니다.

  # csv 확장자와 txt 파일인 경우의 대응을 위해 확장자를 setting 변수에 저장해줍니다.
  setting <- as.character(tools::file_ext(text))

  # 임의의 sep 인자를 지정한 경우 혹은 csv나 txt 이외의 확장자를 가진 파일의 경우
  # read.table 함수로 임의의 인자를 넘겨 줄 수 있도록 설정유형을 custom 이라고 해줍니다.
  if(sep != "" | !(setting  %in% c("csv", "txt")) ) setting <- "custom"

  # csv 인 경우 구분자를 , 쉼표 txt 파일인 경우 \n 개행문자로 세팅합니다.
  # \t 탭 등의 임의의 구분자 지정 시에는 임의의 구분자를 그대로 전달합니다.
  separate <- list(csv = ",", txt = "\n", custom = sep)

  # 결과 값을 전달해줍니다.
  result <- read.table(text, sep = separate[[setting]], fileEncoding = encoding, ...)

  return(result)

}


