
#include "dg_heap/heap.h"
#include <stdint.h>
#include <stdlib.h>
#include <memory>
#include <atomic>
#include "dg_heap/dense_hash_map/dense_hash_map.hpp"

namespace dg::allocation{

    using header_type                               = uint32_t;
    static inline constexpr size_t ALLOCATOR_COUNT  = 4;
    static inline constexpr size_t BINARY_HEIGHT    = 28;
    static inline constexpr size_t LEAF_SZ          = sizeof(std::max_align_t) * 2;

    template <class WorkOrder>
    auto atomic_do(std::atomic_flag& flag, WorkOrder wo){

        while (!flag.test_and_set(std::memory_order_acq_rel)){}
        
        if constexpr(std::is_same_v<void, decltype(wo())>){
            wo();
            flag.clear(std::memory_order_release);
        } else{
            auto rs = wo();
            flag.clear(std::memory_order_release);
            return rs;
        }
    }

    template <class header_type, size_t LEAF_SZ>
    class Allocator{

        private:

            std::shared_ptr<char[]> management_buf; 
            std::unique_ptr<char[]> buf;
            std::unique_ptr<dg::heap::core::Allocatable> allocator; //devirt + premature optis ~= 10% improvement (incl prefetching (memory writing operation))
            std::unique_ptr<std::atomic_flag> bool_flag; //unique ptr for cache isolation + alignment +  initialization

        public:
            
            Allocator() = default; 

            explicit Allocator(std::shared_ptr<char[]> management_buf,
                               std::unique_ptr<char[]> buf,
                               std::unique_ptr<dg::heap::core::Allocatable> allocator,
                               std::unique_ptr<std::atomic_flag> bool_flag): management_buf(std::move(management_buf)),
                                                                             buf(std::move(buf)),
                                                                             allocator(std::move(allocator)),
                                                                             bool_flag(std::move(bool_flag)){}
            
            void * malloc(size_t blk_sz) noexcept{
                
                if (blk_sz == 0u){
                    return nullptr;
                }
                                
                size_t adj_blk_sz               = blk_sz + sizeof(header_type);
                size_t resp_b                   = 0u;
                size_t resp_sz                  = 0u; 
                size_t requesting_node_count    = adj_blk_sz / LEAF_SZ + size_t{adj_blk_sz % LEAF_SZ != 0}; 
                auto resp                       = atomic_do(*this->bool_flag, [&]{return this->allocator->alloc(requesting_node_count);}); //custom devirtualization here (very hard) - need to inspect whether this is mem-bound (stack variable declarations - affect locality) or cond-bound (vtable)
                
                if (!resp){
                    return nullptr;
                }

                std::tie(resp_b, resp_sz) = resp.value();
                char * rs = this->buf.get() + (LEAF_SZ * resp_b + sizeof(header_type));
                this->write_header(static_cast<void *>(rs), static_cast<header_type>(resp_sz)); // >= 20% memory saving if do encoded pointer addr + 10% runtime saving (if pure allocation no initialization - usually not the case)
                
                return static_cast<void *>(rs);
            }

            void free(void * ptr) noexcept{
                
                size_t offs = std::distance(this->buf.get() + sizeof(header_type), static_cast<char *>(ptr)) / LEAF_SZ;
                size_t sz   = this->extract_header(this->buf.get() + std::distance(this->buf.get(), static_cast<char *>(ptr))); //compliance - unknown void * can be casted to char * but the resulting var could not be used for pointer arithmetic (strict C++ rule)
                atomic_do(*this->bool_flag, [&]{this->allocator->free({offs, sz});});
            }
        
        private:

            void write_header(void * ptr, header_type sz) noexcept{

                char * addr = static_cast<char *>(ptr) - sizeof(header_type); 
                std::memcpy(addr, &sz, sizeof(header_type));
            }

            auto extract_header(void * ptr) noexcept -> header_type{
                
                header_type sz{};
                char * addr = static_cast<char *>(ptr) - sizeof(header_type); 
                std::memcpy(&sz, addr, sizeof(header_type));

                return sz;
            }
    };
    
    template <size_t ALLOCATOR_COUNT, class header_type, size_t LEAF_SZ>
    class MultiThreadAllocator{

        private:

            jg::dense_hash_map<std::thread::id, size_t> id_to_idx_map;
            std::array<Allocator<header_type, LEAF_SZ>, ALLOCATOR_COUNT> allocator_vec;

        public:

            static_assert(ALLOCATOR_COUNT <= std::numeric_limits<uint8_t>::max());

            MultiThreadAllocator() = default;

            explicit MultiThreadAllocator(jg::dense_hash_map<std::thread::id, size_t> id_to_idx_map,
                                          std::array<Allocator<header_type, LEAF_SZ>, ALLOCATOR_COUNT>  allocator_vec): id_to_idx_map(std::move(id_to_idx_map)),
                                                                                                                        allocator_vec(std::move(allocator_vec)){}

            void * malloc(size_t blk_sz) noexcept{
                
                if (blk_sz == 0u){ 
                    return nullptr;
                }
                
                const auto& map = this->id_to_idx_map;
                auto idx_ptr    = map.find(std::this_thread::get_id());

                if (idx_ptr == map.end()){
                    std::abort(); 
                }

                uint8_t idx     = static_cast<uint8_t>(idx_ptr->second);
                void * rs       = this->allocator_vec[idx].malloc(blk_sz + sizeof(uint8_t));

                if (!rs){
                    return nullptr;
                }

                std::memcpy(rs, &idx, sizeof(uint8_t));
                return static_cast<char *>(rs) + sizeof(uint8_t);
            }

            void free(void * ptr) noexcept{

                if (!ptr){
                    return;
                }

                void * prev = static_cast<char *>(ptr) - sizeof(uint8_t); //ub
                uint8_t idx = {};

                std::memcpy(&idx, prev, sizeof(uint8_t));
                this->allocator_vec[idx].free(prev);
            }

    };
    
    static inline MultiThreadAllocator<ALLOCATOR_COUNT, header_type, LEAF_SZ> allocator;

    void init(const std::vector<std::thread::id>& ids){

        std::array<Allocator<header_type, LEAF_SZ>, ALLOCATOR_COUNT> allocator_vec{};
        jg::dense_hash_map<std::thread::id, size_t> id_to_idx_map{};

        for (const auto& thr_id: ids){
            id_to_idx_map[thr_id] = id_to_idx_map.size() % ALLOCATOR_COUNT;
        }

        [&]<size_t ...IDX>(const std::index_sequence<IDX...>){
            (
                [&]{
                    (void) IDX;
                    auto management_buf = dg::heap::user_interface::make(BINARY_HEIGHT);
                    auto manager        = dg::heap::user_interface::get_allocator_x(management_buf.get(), std::integral_constant<size_t, IDX>{});
                    auto bool_flag      = std::make_unique<std::atomic_flag>(0);
                    size_t buf_sz       = (size_t{1} << (BINARY_HEIGHT - 1)) * LEAF_SZ + sizeof(header_type);
                    auto buf            = std::make_unique<char[]>(buf_sz);
                    allocator_vec[IDX]  = Allocator<header_type, LEAF_SZ>(std::move(management_buf), std::move(buf), std::move(manager), std::move(bool_flag));
                }(), ...
            );
        }(std::make_index_sequence<ALLOCATOR_COUNT>{});

        allocator = MultiThreadAllocator<ALLOCATOR_COUNT, header_type, LEAF_SZ>(std::move(id_to_idx_map), std::move(allocator_vec));
    }

    void deinit(){

        allocator = {};
    }

    void * malloc(size_t blk_sz){

        return allocator.malloc(blk_sz); 
    }

    void free(void * ptr){

        allocator.free(ptr);
    }
}

