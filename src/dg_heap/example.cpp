
#include "heap.h"
#include <iostream>
#include "assert.h"
#include <random>
#include <chrono>

int main(){

    using namespace std::chrono;

    constexpr size_t HEIGHT         = 28;
    constexpr size_t BASE_LENGTH    = size_t{1} << (HEIGHT - 1);

    std::shared_ptr<char[]> buf = dg::heap::user_interface::make(HEIGHT);
    std::shared_ptr<dg::heap::core::Allocatable> allocator  = dg::heap::user_interface::get_allocator_x(buf.get());
    std::vector<dg::heap::types::interval_type> ptrs{};

    size_t c = 0;
    size_t total_iter = size_t{1} << 28;

    auto b = std::chrono::high_resolution_clock::now(); 

    while (total_iter){
        auto ptr = allocator->alloc(1);
        allocator->free(*ptr);
        --total_iter;
    }

    auto e = std::chrono::high_resolution_clock::now();
    std::cout << std::chrono::duration_cast<std::chrono::milliseconds>(e - b).count() << "<>" << c << std::endl;
}