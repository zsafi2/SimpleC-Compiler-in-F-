### **SimpleC Compiler in F#**
#### *A lexer and parser implementation for SimpleC programs.*

---

## **Table of Contents**
- [Description](#description)
- [Features](#features)
- [Installation](#installation)
- [Usage](#usage)
- [Modules](#modules)
- [Technologies Used](#technologies-used)
- [Future Improvements](#future-improvements)
- [Contributing](#contributing)
- [License](#license)

---

## **Description**
The SimpleC Compiler is an F# project that implements a **lexer** and **parser** for the SimpleC programming language. The lexer converts input SimpleC source code into a list of tokens, while the parser validates the syntax of the program against the SimpleC grammar. The program determines whether a given SimpleC program is valid by checking its structure according to formal syntax rules.

---

## **Features**
- **Lexical Analysis**: Tokenizes SimpleC source code.
- **Syntax Checking**: Parses tokenized input and verifies adherence to SimpleC's grammar.
- **Error Handling**: Provides detailed syntax error messages.
- **Support for Basic Statements**: Recognizes assignments, variable declarations, conditional statements, and I/O operations.

---

## **Installation**
### **Prerequisites**
Ensure you have the following installed:
- **.NET SDK** (for F# development)
- **F# Compiler (fsharpc)**

### **Clone the Repository**
```bash
git clone https://github.com/yourusername/simplec-compiler.git
cd simplec-compiler
```

### **Compile the Code**
```bash
fsharpc -o SimpleC.exe lexer.fs parser.fs main.fs
```

---

## **Usage**
Run the compiled program with:
```bash
dotnet run <filename>.simplec
```
The program will analyze the provided SimpleC source file and output **"Success!"** if the program is valid or an error message otherwise.

---

## **Modules**
| Module | Description |
|--------|------------|
| `lexer.fs` | Converts input SimpleC code into a list of tokens. |
| `parser.fs` | Parses the tokenized input and validates syntax. |
| `main.fs` | Handles file input and integrates the lexer and parser. |

---

## **Technologies Used**
- **F#**: Functional programming language.
- **.NET SDK**: Compilation and execution environment.

---

## **Future Improvements**
- Extend syntax support to **loops and functions**.
- Implement **semantic analysis**.
- Improve error handling with more detailed debugging output.
- Optimize performance for large SimpleC programs.

---

## **Contributing**
Contributions are welcome! If you’d like to improve this project:
1. Fork the repository.
2. Create a feature branch (`git checkout -b feature-name`).
3. Commit your changes (`git commit -m "Add feature"`).
4. Push to the branch (`git push origin feature-name`).
5. Open a Pull Request.

