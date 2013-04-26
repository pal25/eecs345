description: Test basic static function calls
value: 10

class Main {
    static test(x) {
	return x + 2;
    }

    static main() {
	return test(8);
    }
}
