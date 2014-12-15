/*
<testinfo>
test_generator="config/mercurium-cxx14"
</testinfo>
*/

auto l = [](auto x) -> float { return x + 1; };

float (*pf1)(int x) = l;
float (*pf2)(float x) = l;
