#include <iostream>

int main()
{
    std::cin.tie(nullptr);
    std::ios_base::sync_with_stdio(false);
    
    int arr1[100000], arr2[100000];
    int n = 0, a, b;
    while(std::cin >> a >> b)
    {
        arr1[n] = a;
        arr2[n ++] = b;
    }

    std::cout << '[' << arr1[0];
    for(int i = 1; i < n; ++ i)
    {
        std::cout << ',' << arr1[i];
    }
    std::cout << "] [" << arr2[0];
    for(int i = 1; i < n; ++ i)
    {
        std::cout << ',' << arr2[i];
    }
    std::cout << "]\n";

    std::cout << n << '\n';

    return  0;
}