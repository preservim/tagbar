using Foo.Sub;

public class GlobalTestClass {
	public GlobalTestClass() {
	}
}

namespace Maman {
	static int main () {
		stdout.printf ("Namespace Test\n");

		Bar.run ();

		new GlobalTestClass();

		var obj = new ClassInNestedNamespace ();

		return 0;
	}

	class Bar : Object {
		public static void run () {
			stdout.printf ("Class in Namespace Test\n");
		}
	}
}

public class Foo.Sub.ClassInNestedNamespace {
}


void main () {
	Maman.main ();
}

