
#ifndef __DG_ALLOCATION_H__
#define __DG_ALLOCATION_H__

#include "dg_heap/heap.h"
#include <stdint.h>
#include <stdlib.h>
#include <memory>
#include <atomic>
#include "dg_heap/dense_hash_map/dense_hash_map.hpp"
#include <thread>
#include "assert.h"
#include <bit>

namespace dg::allocation{

    using ptr_type                                          = uint64_t;
    using alignment_type                                    = uint16_t;
    
    static inline constexpr size_t PTROFFS_BSPACE           = sizeof(uint32_t) * CHAR_BIT;
    static inline constexpr size_t PTRSZ_BSPACE             = sizeof(uint16_t) * CHAR_BIT;
    static inline constexpr size_t ALLOCATOR_ID_BSPACE      = sizeof(uint8_t) * CHAR_BIT;
    static inline constexpr size_t ALIGNMENT_BSPACE         = sizeof(uint8_t) * CHAR_BIT;

    static inline constexpr ptr_type NULLPTR                = ptr_type{0u}; 
    static inline constexpr size_t ALLOCATOR_COUNT          = 1;
    static inline constexpr size_t BINARY_HEIGHT            = 19;
    static inline constexpr size_t LEAF_SZ                  = 16;
    static inline constexpr size_t DEFLT_ALIGNMENT          = 8; 

    static_assert(PTROFFS_BSPACE + PTRSZ_BSPACE + ALLOCATOR_ID_BSPACE + ALIGNMENT_BSPACE <= sizeof(ptr_type) * CHAR_BIT);
    static_assert(-1 == ~0u);
    static_assert(!NULLPTR);
    static_assert(std::add_pointer_t<void>(nullptr) == reinterpret_cast<void *>(0u));

    template <class T, size_t SZ, std::enable_if_t<std::is_unsigned_v<T>, bool> = true>
    constexpr auto low(const std::integral_constant<size_t, SZ>) noexcept -> T{
        
        static_assert(SZ <= sizeof(T) * CHAR_BIT);

        if constexpr(SZ == sizeof(T) * CHAR_BIT){
            return ~T{0u};
        } else{
            return (T{1u} << SZ) - 1; 
        }
    }

    constexpr auto is_pow2(size_t val) noexcept -> bool{

        return val != 0u && (val & (val - 1)) == 0u;
    }

    inline auto align(void * ptr, const uintptr_t alignment) noexcept -> void *{

        assert(is_pow2(alignment));

        const uintptr_t fwd_sz = alignment - 1;
        const uintptr_t mask   = ~fwd_sz; 

        return reinterpret_cast<void *>((reinterpret_cast<uintptr_t>(ptr) + fwd_sz) & mask);
    }

    template <class WorkOrder>
    inline auto atomic_do(std::atomic_flag& flag, const WorkOrder& wo){

        while (!flag.test_and_set(std::memory_order_acq_rel)){}
        
        if constexpr(std::is_same_v<void, decltype(wo())>){
            wo();
            flag.clear(std::memory_order_acq_rel);
        } else{
            auto rs = wo();
            flag.clear(std::memory_order_acq_rel);
            return rs;
        }
    }

    class Allocator{

        private:

            std::shared_ptr<char[]> management_buf; 
            std::shared_ptr<char[]> buf;
            std::unique_ptr<dg::heap::core::Allocatable> allocator; //weird bug (need inspection)
            std::unique_ptr<std::atomic_flag> bool_flag;

        public:
            
            Allocator() = default; 

            explicit Allocator(std::shared_ptr<char[]> management_buf,
                               std::shared_ptr<char[]> buf,
                               std::unique_ptr<dg::heap::core::Allocatable> allocator,
                               std::unique_ptr<std::atomic_flag> bool_flag): management_buf(std::move(management_buf)),
                                                                             buf(std::move(buf)),
                                                                             allocator(std::move(allocator)),
                                                                             bool_flag(std::move(bool_flag)){}
            
            inline auto malloc(size_t blk_sz) noexcept -> ptr_type{
                
                size_t req_node_sz = blk_sz / LEAF_SZ + size_t{blk_sz % LEAF_SZ != 0}; 
                return this->malloc_node(req_node_sz);
            }

            template <size_t BLK_SZ>
            inline auto malloc(const std::integral_constant<size_t, BLK_SZ>) noexcept -> ptr_type{

                constexpr size_t req_node_sz = BLK_SZ / LEAF_SZ + size_t{BLK_SZ % LEAF_SZ != 0};
                return this->malloc_node(req_node_sz);
            }

            inline void free(ptr_type ptr_addr) noexcept{
                
                if (!ptr_addr){
                    return;
                }

                auto [offs, sz] = decode_ptr(ptr_addr);  
                atomic_do(*this->bool_flag, [&]{this->allocator->free({offs, sz});});
            }

            inline auto c_addr(ptr_type ptr_addr) noexcept -> void *{

                if (!ptr_addr){
                    return nullptr;
                }

                auto [offs, _] = decode_ptr(ptr_addr);
                char * rs = this->buf.get();
                std::advance(rs, offs * LEAF_SZ);

                return rs;
            }
        
        private:

            inline auto malloc_node(const size_t node_sz) noexcept -> ptr_type{

                auto resp = atomic_do(*this->bool_flag, [&]{return this->allocator->alloc(node_sz);});

                if (!resp){
                    return NULLPTR;
                }

                auto [resp_offs, resp_sz] = resp.value();                
                return encode_ptr(resp_offs, resp_sz);
            } 

            inline auto encode_ptr(auto hi, auto lo) const noexcept -> ptr_type{
                
                return (static_cast<ptr_type>(hi) << PTRSZ_BSPACE) | static_cast<ptr_type>(lo + 1);
            }

            inline auto decode_ptr(ptr_type ptr) const noexcept -> std::pair<ptr_type, ptr_type>{

                ptr_type hi = ptr >> PTRSZ_BSPACE;
                ptr_type lo = ptr & low<ptr_type>(std::integral_constant<size_t, PTRSZ_BSPACE>{});

                return {hi, lo - 1};
            }
    };
    
    class MultiThreadAllocator{

        private:

            jg::dense_hash_map<std::thread::id, size_t> id_to_idx_map;
            std::array<Allocator, ALLOCATOR_COUNT> allocator_vec;

        public:

            MultiThreadAllocator() = default;

            explicit MultiThreadAllocator(jg::dense_hash_map<std::thread::id, size_t> id_to_idx_map,
                                          std::array<Allocator, ALLOCATOR_COUNT>  allocator_vec): id_to_idx_map(std::move(id_to_idx_map)),
                                                                                                  allocator_vec(std::move(allocator_vec)){}

            inline auto malloc(size_t blk_sz) noexcept -> ptr_type{

                size_t thr_id   = this->get_cur_thr_id();
                ptr_type rs     = this->allocator_vec[thr_id].malloc(blk_sz);

                if (!rs){
                    return NULLPTR;
                }

                return encode_ptr(rs, thr_id);
            }

            template <size_t BLK_SZ>
            inline auto malloc(const std::integral_constant<size_t, BLK_SZ>) noexcept -> ptr_type{

                size_t thr_id   = this->get_cur_thr_id();
                ptr_type rs     = this->allocator_vec[thr_id].malloc(std::integral_constant<size_t, BLK_SZ>{});

                if (!rs){
                    return NULLPTR;
                }

                return encode_ptr(rs, thr_id);
            }

            inline void free(ptr_type ptr) noexcept{
                
                if (!ptr){
                    return;
                }

                auto [pptr, thr_id] = decode_ptr(ptr);
                this->allocator_vec[thr_id].free(pptr);
            }

            inline auto c_addr(ptr_type ptr) noexcept -> void *{

                if (!ptr){
                    return nullptr;
                }

                auto [pptr, thr_id] = decode_ptr(ptr);
                return this->allocator_vec[thr_id].c_addr(pptr);
            }
        
        private:

            inline auto get_cur_thr_id() const noexcept -> size_t{

                return static_cast<size_t>(this->id_to_idx_map.find(std::this_thread::get_id())->second); //
            } 

            inline auto encode_ptr(auto hi, auto lo) const noexcept -> ptr_type{

                return (static_cast<ptr_type>(hi) << ALLOCATOR_ID_BSPACE) | static_cast<ptr_type>(lo);
            }

            inline auto decode_ptr(ptr_type ptr) const noexcept -> std::pair<ptr_type, ptr_type>{

                ptr_type hi = ptr >> ALLOCATOR_ID_BSPACE;
                ptr_type lo = ptr & low<ptr_type>(std::integral_constant<size_t, ALLOCATOR_ID_BSPACE>{}); 

                return {hi, lo};
            }
    };
    
    static inline MultiThreadAllocator allocator;

    void init(const std::vector<std::thread::id>& ids){

        assert(ids.size() <= low<ptr_type>(std::integral_constant<size_t, ALLOCATOR_ID_BSPACE>{}));

        std::array<Allocator, ALLOCATOR_COUNT> allocator_vec{};
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
                    size_t buf_sz       = (size_t{1} << (BINARY_HEIGHT - 1)) * LEAF_SZ;
                    auto buf            = std::unique_ptr<char[], decltype(&std::free)>(static_cast<char *>(std::aligned_alloc(LEAF_SZ, buf_sz)), &std::free);
                    if (!buf.get()){
                        throw std::bad_alloc();
                    }
                    allocator_vec[IDX]  = Allocator(std::move(management_buf), std::move(buf), std::move(manager), std::move(bool_flag));
                }(), ...
            );
        }(std::make_index_sequence<ALLOCATOR_COUNT>{});

        allocator = MultiThreadAllocator(std::move(id_to_idx_map), std::move(allocator_vec));
    }

    void deinit() noexcept{

        allocator = {}; //
    }

    inline auto malloc(size_t blk_sz, size_t alignment) noexcept -> ptr_type{

        assert(is_pow2(alignment));

        size_t fwd_mul_factor   = std::max(static_cast<size_t>(alignment), static_cast<size_t>(LEAF_SZ)) / LEAF_SZ - 1; 
        size_t adj_blk_sz       = blk_sz + fwd_mul_factor * LEAF_SZ;
        ptr_type ptr            = allocator.malloc(adj_blk_sz);

        if (!ptr){
            return NULLPTR;
        }

        ptr <<= ALIGNMENT_BSPACE;
        ptr |= static_cast<ptr_type>(std::countr_zero(static_cast<alignment_type>(alignment)));

        return ptr;
    } 

    template <size_t BLK_SZ, size_t ALIGNMENT>
    inline auto malloc(const std::integral_constant<size_t, BLK_SZ>, const std::integral_constant<size_t, ALIGNMENT>) noexcept -> ptr_type{

        static_assert(is_pow2(ALIGNMENT));

        constexpr size_t fwd_mul_factor = std::max(ALIGNMENT, LEAF_SZ) / LEAF_SZ - 1;
        constexpr size_t adj_blk_sz     = BLK_SZ + fwd_mul_factor * LEAF_SZ;
        ptr_type ptr                    = allocator.malloc(std::integral_constant<size_t, adj_blk_sz>{});

        if (!ptr){
            return NULLPTR;
        }

        ptr <<= ALIGNMENT_BSPACE;
        ptr |= static_cast<ptr_type>(std::countr_zero(static_cast<alignment_type>(ALIGNMENT)));

        return ptr;
    }

    inline auto malloc(size_t blk_sz) noexcept -> ptr_type{

        return malloc(blk_sz, DEFLT_ALIGNMENT); 
    }

    template <size_t BLK_SZ>
    inline auto malloc(const std::integral_constant<size_t, BLK_SZ>) noexcept -> ptr_type{

        return malloc(std::integral_constant<size_t, BLK_SZ>{}, std::integral_constant<size_t, DEFLT_ALIGNMENT>{});
    }

    inline auto cppmalloc(size_t blk_sz, alignment_type alignment) -> ptr_type{

        if (auto rs = malloc(blk_sz, alignment); rs){
            return rs;
        }

        throw std::bad_alloc();
    }

    template <size_t BLK_SZ, size_t ALIGNMENT>
    inline auto cppmalloc(const std::integral_constant<size_t, BLK_SZ>, const std::integral_constant<size_t, ALIGNMENT>) -> ptr_type{

        if (auto rs = malloc(std::integral_constant<size_t, BLK_SZ>{}, std::integral_constant<size_t, ALIGNMENT>{}); rs){
            return rs;
        }

        throw std::bad_alloc();
    } 

    inline auto cppmalloc(size_t blk_sz) -> ptr_type{

        return cppmalloc(blk_sz, DEFLT_ALIGNMENT);
    }

    template <size_t BLK_SZ>
    inline auto cppmalloc(const std::integral_constant<size_t, BLK_SZ>) -> ptr_type{

        return cppmalloc(std::integral_constant<size_t, BLK_SZ>{}, std::integral_constant<size_t, DEFLT_ALIGNMENT>{});
    }

    inline auto c_addr(ptr_type ptr) noexcept -> void *{
        
        size_t alignment_log2   = ptr & low<ptr_type>(std::integral_constant<size_t, ALIGNMENT_BSPACE>{}); 
        size_t alignment        = size_t{1} << alignment_log2; 
        ptr_type pptr           = ptr >> ALIGNMENT_BSPACE; 

        return align(allocator.c_addr(pptr), alignment); //assumption (not logically stable)
    }

    inline void free(ptr_type ptr) noexcept{

        allocator.free(ptr >> ALIGNMENT_BSPACE); //assumption (not logically stable)
    }
}

#endif