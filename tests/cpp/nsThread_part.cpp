namespace {
struct nsThreadShutdownContext {
  nsThread *joiningThread;
  PRBool    shutdownAck;
  union {
    int a;
    int b;
    union {
        char f;
        char g;
    };
  };
};
};

struct teststruct {
    char foo;
    union {
        int x;
        int y;
    };
};

namespace
{
  short int i = 0;

  class Testclass
  {
    int j;

  public:

    Testclass();
    ~Testclass();
  };

  namespace
  {
    string str = "";
    class FooClass
    {
        FooClass();
    };
  }
};
