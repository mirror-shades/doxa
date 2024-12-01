import unittest
from io import StringIO

class TestFizzBuzz(unittest.TestCase):
    def test_fizzbuzz_output(self):
        # Expected output based on the rules:
        # - Numbers divisible by 3: "fizz"
        # - Numbers divisible by 5: "buzz"
        # - Numbers divisible by both 3 and 5: "fizzbuzz"
        # - Other numbers: the number itself
        expected_output = []
        for i in range(101):  # 0 to 100
            if i == 0:
                continue
            elif i % 15 == 0:
                expected_output.append("fizzbuzz")
            elif i % 3 == 0:
                expected_output.append("fizz")
            elif i % 5 == 0:
                expected_output.append("buzz")
            else:
                expected_output.append(str(i))
        
        # Convert the expected output to a single string with newlines
        expected = "\n".join(expected_output) + "\n"
        
        # Add debug prints
        output = self.get_program_output()
        
        # Compare the outputs
        self.assertEqual(output, expected)
    
    @staticmethod
    def strip_value(line):
        # Get the last word after the last '=' if it exists
        if '=' in line:
            value = line.split('=')[-1].strip()
            # Remove quotes if they exist
            return value.strip('"')
        return line.strip()
    
    def get_program_output(self):
        import subprocess
        try:
            result = subprocess.run(['./zig-out/bin/doxa', './test/misc/fizzbuzz.doxa'], 
                                  capture_output=True, 
                                  text=True,
                                  check=True)
            # Process each line through strip_value and join with newlines
            output_lines = [self.strip_value(line) for line in result.stdout.splitlines()]
            return '\n'.join(output_lines) + '\n'
        except subprocess.CalledProcessError as e:
            print(f"Error running program: {e}")
            print(f"stderr: {e.stderr}")
            raise
        except FileNotFoundError:
            print("Error: Could not find the executable. Make sure the path is correct.")
            raise

if __name__ == '__main__':
    unittest.main()
