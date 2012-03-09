import Parser (parser)
import System( getArgs )


main = do 
	cont <- getContents
	print $ parser cont

