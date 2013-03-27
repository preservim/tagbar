interface Foo : Object {
	public void foo<T> (owned T bar) {
	       bar = null;
	}
}

class Baz : Object, Foo {
}

void foo<T> (owned T bar) {
	bar = null;
}

void main () {
	var bar = new Object ();
	foo<Object> (bar);
	assert (bar.ref_count == 1);

	var baz = new Baz ();
	baz.foo<Object> (bar);
	assert (baz.ref_count == 1);
}
