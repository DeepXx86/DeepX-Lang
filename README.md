

## DeepX-Lang is the first programming language I created 😳😅 ( Project is not finished. 💀🤫 )

-This programming language is simple, and I don't think it can be used in real projects. 🤓

-inspiration --> (javascript, lua, python, haskell, go-lang, uwupp,z#)

# Examples ->

### Print 🤡
~~~~
write("Hello from DeepX")
~~~~
### Wait 🤓
~~~~
a = 0
while ( a < 100 ){
    write("wait test")
    wait(1000) // 1000 = 1 second because it's a microsecond.
}
~~~~
### User_Input 🥱
~~~~
func Random(){
    input = getLine("Chosse 1-10 : ")
    if (input == 10){
        write("lucky")
    }
    else {
        a = 0
        while ( a < 1000000 ){
            write("LOL")
            wait(1)
        }
    }

}

Random()
~~~~
### If / Else 😳
~~~~
func If_Else(){
    write("Give me a-z")
    Skibidi = getLine("( a - z only ): ")
    if ( Skibidi == "z" ){
        write("Correct")
    }
    else {
        write("nah it", "z")
    }
}

If_Else()
~~~~

## Creating Your Own Programming Language Using My Source Code
- Install Stack & Haskell [Install Stack & Haskell](https://www.haskell.org/downloads/)
- Create a new project --> stack new DeepX && cd DeepX
- Update stack.yaml --> resolver: lts-18.28
- Delete the app folder & Create a new Main.hs file in the src directory
Tree :
~~~~
Deepx/
├── src/
│   ├── AST.hs
│   ├── Lexer.hs
│   ├── Parser.hs
│   ├── Evaluator.hs
│   └── Main.hs
├── package.yaml
└── stack.yaml
~~~~

