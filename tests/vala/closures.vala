delegate int Func ();

[CCode (has_target = false)]
delegate void NoTargetFunc ();

int A (int k, Func x1, Func x2, Func x3, Func x4, Func x5) {
	Func B = null;
	B = () => {
		k = k - 1;
		return A (k, B, x1, x2, x3, x4);
	};
	return k <= 0 ? x4 () + x5 () : B ();
}

void B ([CCode (array_length = false, array_null_terminated = true)] int[] array, NoTargetFunc func) {
	Func C = () => { array = null; func (); return 0; };
}

void main () {
	int result = A (10, () => 1, () => -1, () => -1, () => 1, () => 0);
	assert (result == -67);
}

