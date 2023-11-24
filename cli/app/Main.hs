import Core
import Promise qualified


main :: MainFunction
main = Promise.runAsMain do
  print "I know kung fu."