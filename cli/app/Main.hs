import Core
import Neo qualified
import Promise qualified


main :: MainFunction
main = Promise.runAsMain Neo.init