import subprocess
import sys

def run_lexer_test():
    try:
        # Run the doxa command with --debug-lexer flag
        result = subprocess.run(['./zig-out/bin/doxa', '--debug-lexer', './test.doxa'], 
                              capture_output=True, 
                              text=True)
        
        # Check if the command was successful
        if result.returncode != 0:
            print(f"Error running doxa: {result.stderr}")
            return False

        output_lines = result.stderr.strip().split('\n')

        expected_tokens = [
            ('SLASH', '/'),
            ('SLASH_SLASH', '//'),
            ('SLASH_EQUAL', '/='),
            ('LEFT_PAREN', '('),
            ('RIGHT_PAREN', ')'),
            ('LEFT_BRACE', '{'),
            ('RIGHT_BRACE', '}'),
            ('LEFT_BRACKET', '['),
            ('RIGHT_BRACKET', ']'),
            ('COMMA', ','),
            ('DOT', '.'),
            ('SEMICOLON', ';'),
            ('MODULO', '%'),
            ('ASTERISK', '*'),
            ('ASTERISK_EQUAL', '*='),
            ('POWER', '^'),
            ('POWER_EQUAL', '^='),
            ('PLUS', '+'),
            ('PLUS_PLUS', '++'),
            ('PLUS_EQUAL', '+='),
            ('MINUS', '-'),
            ('MINUS_MINUS', '--'),
            ('MINUS_EQUAL', '-='),
            ('EQUAL', '='),
            ('EQUAL_EQUAL', '=='),
            ('BANG', '!'),
            ('BANG_EQUAL', '!='),
            ('GREATER', '>'),
            ('GREATER_EQUAL', '>='),
            ('LESS', '<'),
            ('LESS_EQUAL', '<='),
            ('VAR', 'var'),
            ('CONST', 'const'),
            ('FUNCTION', 'fn'),
            ('RETURN', 'return'),
            ('BREAK', 'break'),
            ('CONTINUE', 'continue'),
            ('THROW', 'throw'),
            ('TRY', 'try'),
            ('CATCH', 'catch'),
            ('WHILE', 'while'),
            ('FOR', 'for'),
            ('FOREACH', 'foreach'),
            ('IN', 'in'),
            ('IF', 'if'),
            ('THEN', 'then'),
            ('ELSE', 'else'),
            ('AND', 'and'),
            ('OR', 'or'),
            ('IDENTIFIER', 'myVariable'),
            ('INT', '42'),
            ('FLOAT', '3.14'),
            ('STRING', '"hello"'),
            ('BOOL', 'true'),
            ('NOTHING', 'nothing'),
            # Array example tokens
            ('LEFT_BRACKET', '['),
            ('INT', '1'),
            ('COMMA', ','),
            ('INT', '2'),
            ('COMMA', ','),
            ('INT', '3'),
            ('RIGHT_BRACKET', ']'),
            # End of file char
            ('EOF', ''),
        ]
        
        # Check each output line against expected tokens
        for i, (expected_type, expected_lexeme) in enumerate(expected_tokens):
            if i >= len(output_lines):
                print(f"Missing expected token: {expected_type} '{expected_lexeme}'")
                return False
                
            line = output_lines[i]
            if not (f"token.TokenType.{expected_type}" in line and f"'{expected_lexeme}'" in line):
                print(f"Mismatch at line {i+1}:")
                print(f"Expected: token.TokenType.{expected_type} '{expected_lexeme}'")
                print(f"Got: {line}")
                return False
        
        print("All tokens matched successfully!")
        return True
        
    except FileNotFoundError:
        print("Error: Could not find doxa executable. Make sure you're in the correct directory.")
        return False
    except Exception as e:
        print(f"Unexpected error: {e}")
        return False

if __name__ == "__main__":
    success = run_lexer_test()
    sys.exit(0 if success else 1)
