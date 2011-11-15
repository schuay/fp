all (\x -> (unlines.lines) x /= x) ctxlines -- unlines.lines != x
(((<)2).length) ctxlines -- ctxlines has more than 2 entries
any (((<)2).length.lines) ctxlines -- at least one entry must have more than 2 lines

all (\x -> (lines.unlines) x /= x) ctxunlines -- unlines.lines != x
(((<)2).length) ctxunlines -- ctxunlines has more than 2 entries
any (((<)2).length) ctxunlines -- at least one entry must have more than 2 lines

all (\x -> (unwords.words) x /= x) ctxwords -- unwords.words != x
(((<)2).length) ctxwords -- ctxwords has more than 2 entries
any (((<)2).length.words) ctxwords -- at least one entry must have more than 2 words

all (\x -> (words.unwords) x /= x) ctxunwords -- unwords.words != x
(((<)2).length) ctxunwords -- ctxunwords has more than 2 entries
any (((<)2).length) ctxunwords -- at least one entry must have more than 2 words

unixtac "a\nb\nc\n" == "c\nb\na\n"
unixtac "a" == "a\n"
unixtac "a\na" == "a\na\n"
unixtac "a \na" == "a\na \n"
unixtac "" == ""

unixhead (-1) "a\nb\nc\n" == ""
unixhead 0 "a\nb\nc\n" == ""
unixhead 5 "a\nb\nc\n" == "a\nb\nc\n"
unixhead 2 "a\nb\nc\n" == "a\nb\n"

unixtail (-1) "a\nb\nc\n" == ""
unixtail 0 "a\nb\nc\n" == ""
unixtail 2 "a\nb\nc\n" == "b\nc\n"
unixtail 5 "a\nb\nc\n" == "a\nb\nc\n"

unixgrep "" "a \n ab \n abab \n ba" == "a \n ab \n abab \n ba\n"
unixgrep "a" "a \n ab \n abab \n ba" == "a \n ab \n abab \n ba\n"
unixgrep "ab" "a \n ab \n abab \n ba" == " ab \n abab \n"
unixgrep "b" "a \n ab \n abab \n ba" == " ab \n abab \n ba\n"
unixgrep "c" "a \n ab \n abab \n ba" == ""

unixtac' "a\nb\nc\n" == "c\nb\na\n"
unixtac' "a" == "a\n"
unixtac' "a\na" == "a\na\n"
unixtac' "a \na" == "a\na \n"
unixtac' "" == ""

unixhead' (-1) "a\nb\nc\n" == ""
unixhead' 0 "a\nb\nc\n" == ""
unixhead' 5 "a\nb\nc\n" == "a\nb\nc\n"
unixhead' 2 "a\nb\nc\n" == "a\nb\n"

unixtail' (-1) "a\nb\nc\n" == ""
unixtail' 0 "a\nb\nc\n" == ""
unixtail' 2 "a\nb\nc\n" == "b\nc\n"
unixtail' 5 "a\nb\nc\n" == "a\nb\nc\n"

unixgrep' "" "a \n ab \n abab \n ba" == "a \n ab \n abab \n ba\n"
unixgrep' "a" "a \n ab \n abab \n ba" == "a \n ab \n abab \n ba\n"
unixgrep' "ab" "a \n ab \n abab \n ba" == " ab \n abab \n"
unixgrep' "b" "a \n ab \n abab \n ba" == " ab \n abab \n ba\n"
unixgrep' "c" "a \n ab \n abab \n ba" == ""

unixrev "abc \n def" == " cba\nfed \n"
unixrev "abc" == "cba\n"
unixrev "" == ""

wordrev "ab cd ef \n gh ij kl" == "ef cd ab\nkl ij gh\n"
wordrev "ab" == "ab\n"
wordrev "" == ""

unixwcw "" == 0
unixwcw "a" == 1
unixwcw "a b" == 2
unixwcw "a bc" == 2
unixwcw "a bc dddd\n" == 3
unixwcw "a bc \n dddd\n" == 3

unixwc "" == (0, 0, 0)
unixwc "a" == (1, 1, 1)
unixwc "a b" == (1, 2, 3)
unixwc "a bc" == (1, 2, 4)
unixwc "a bc dddd\n" == (1, 3, 10)
unixwc "a bc \n dddd\n" == (2, 3, 12)
