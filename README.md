# Baesich
 Calculator that does funky shit with funky bases


## Notes

parseDigit = choice $ map (try . char') $ ['0'..'9'] ++ ['A'..'Z'] ++ ['.']
  where char' '.' = return (-2)
        char' c = if (GHC.List.null [c]) then (fail "empty string haha") else
            case elemIndex c $ ['0'..'9'] ++ ['A'..'Z'] of
                    Just n -> return $ fromIntegral n
                    Nothing -> fail "Invalid digit"
