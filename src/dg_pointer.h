#ifndef __DG_POINTER_H__
#define __DG_POINTER_H__

#include "dg_allocation.h" 
#include <type_traits>
#include "assert.h"
#include <vector>

namespace dg::pointer{

    using addr_type         = dg::allocation::ptr_type; 
    using ref_counter_type  = size_t; 
    
    template <class T, class = void>
    struct has_dig_operator: std::false_type{};

    template <class T>
    struct has_dig_operator<T, std::void_t<decltype(std::declval<T&>().operator ->())>>: std::true_type{};

    template <class T, class = void> 
    struct has_deref_operator: std::false_type{};

    template <class T>
    struct has_deref_operator<T, std::void_t<decltype(std::declval<T&>().operator *())>>: std::true_type{};

    template <class T, class = void>
    struct has_array_deref_operator: std::false_type{};

    template <class T>
    struct has_array_deref_operator<T, std::void_t<decltype(std::declval<T&>().operator [](intmax_t{}))>>: std::true_type{};

    template <class To, class Fr, class = void>
    struct is_static_ptr_castable: std::false_type{};

    template <class To, class Fr>
    struct is_static_ptr_castable<To, Fr, std::void_t<decltype(static_cast<To *>(std::declval<Fr *>()))>>: std::true_type{};
    
    template <class To, class Fr, class = void>
    struct is_dynamic_ptr_castable: std::false_type{};

    template <class To, class Fr>
    struct is_dynamic_ptr_castable<To, Fr, std::void_t<decltype(dynamic_cast<To *>(std::declval<Fr *>()))>>: std::true_type{};

    template <class To, class Fr, class = void>
    struct is_reinterpret_ptr_castable: std::false_type{};

    template <class To, class Fr>
    struct is_reinterpret_ptr_castable<To, Fr, std::void_t<decltype(reinterpret_cast<To *>(std::declval<Fr *>()))>>: std::true_type{};

    template <class T, class = void>
    struct is_dereferencable: std::false_type{};

    template <class T>
    struct is_dereferencable<T, std::void_t<decltype(*std::declval<T&>())>>: std::true_type{};

    template <class T>
    struct is_bound_array: std::false_type{};

    template <class T, size_t SZ>
    struct is_bound_array<T[SZ]>: std::true_type{};

    template <class T>
    using base_t = std::remove_cv_t<std::remove_reference_t<T>>; //wrong

    template <class Base, class Derived>
    static inline constexpr bool is_strict_base_of_v                = !std::is_same_v<Base, Derived> && std::is_base_of_v<Base, Derived>;
        
    template <class T, class U>
    static inline constexpr bool is_strict_ptr_constructible_fr_v   = is_strict_base_of_v<T, U> && !std::is_array_v<T> && !std::is_array_v<U>;

    template <class T, class U>
    static inline constexpr bool is_ptr_constructible_fr_v          = std::is_same_v<T, U> || is_strict_ptr_constructible_fr_v<T, U>;

    template <class T>
    static inline constexpr bool has_dig_operator_v                 = has_dig_operator<T>::value;

    template <class T>
    static inline constexpr bool has_deref_operator_v               = has_deref_operator<T>::value;

    template <class T>
    static inline constexpr bool has_array_deref_operator_v         = has_array_deref_operator<T>::value;

    template <class To, class Fr>
    static inline constexpr bool is_static_ptr_castable_v           = is_static_ptr_castable<To, Fr>::value;

    template <class To, class Fr>
    static inline constexpr bool is_dynamic_ptr_castable_v          = is_dynamic_ptr_castable<To, Fr>::value;

    template <class To, class Fr>
    static inline constexpr bool is_reinterpret_ptr_castable_v      = is_reinterpret_ptr_castable<To, Fr>::value; 

    template <class T>
    static inline constexpr bool is_bound_array_v                   = is_bound_array<T>::value;

    template <class T>
    static inline constexpr bool is_dereferencable_v                = is_dereferencable<T>::value;

    template <class = void>
    static inline constexpr bool FALSE_VAL                          = false; 

    template <class T>
    class dg_raw_ptr{
        
        private:

            T * ptr;
            addr_type addr;

        public:

            using self = dg_raw_ptr; 

            //implicit T * here - in future deprecate addr_type 

            constexpr dg_raw_ptr() noexcept: ptr(nullptr), addr(dg::allocation::NULLPTR){}
            constexpr dg_raw_ptr(std::nullptr_t) noexcept: dg_raw_ptr(){} 
            constexpr explicit dg_raw_ptr(addr_type addr, T * ptr) noexcept: ptr(ptr), addr(addr){}
            constexpr ~dg_raw_ptr() = default;
            constexpr dg_raw_ptr(const self&) = default;
            constexpr dg_raw_ptr(self&&) = default;
            constexpr auto operator =(const self&) -> self& = default;
            constexpr auto operator =(self&&) -> self& = default; 

            constexpr auto get() const noexcept -> T *{

                return this->ptr;
            }

            constexpr auto heap_addr() const noexcept -> addr_type{

                return this->addr;
            }

            constexpr auto operator ->() const noexcept -> T *{

                assert(static_cast<bool>(this->ptr)); //disable constexpr
                return this->ptr;
            }
            
            template <class _T = T *, std::enable_if_t<std::is_same_v<_T, T *> && is_dereferencable_v<_T>, bool> = true>
            constexpr auto operator *() const noexcept{
                
                return *this->operator->();
            } 

            constexpr operator bool() const noexcept{

                return static_cast<bool>(this->ptr);
            } 
    };

    template <class T>
    class dg_raw_ptr<T[]>{

        private:

            T * ptr;
            addr_type addr;

        public:

            using self = dg_raw_ptr; 

            constexpr dg_raw_ptr() noexcept: ptr(nullptr), addr(dg::allocation::NULLPTR){}
            constexpr dg_raw_ptr(std::nullptr_t) noexcept: dg_raw_ptr(){} 
            constexpr explicit dg_raw_ptr(addr_type addr, T * ptr) noexcept: ptr(ptr), addr(addr){}
            constexpr ~dg_raw_ptr() = default;
            constexpr dg_raw_ptr(const self&) = default;
            constexpr dg_raw_ptr(self&&) = default;
            constexpr auto operator =(const self&) -> self& = default;
            constexpr auto operator =(self&&) -> self& = default; 

            constexpr auto get() const noexcept -> T *{

                return this->ptr;
            }

            constexpr auto heap_addr() const noexcept -> addr_type{

                return this->addr;
            }

            constexpr auto operator[](intmax_t idx) const noexcept -> T&{
                
                assert(static_cast<bool>(this->ptr)); //disable constexpr
                return this->ptr[idx];
            }

            constexpr operator bool() const noexcept{

                return static_cast<bool>(this->ptr);
            } 
    };

    constexpr auto get_least_multiple_of_2_eq_exceeds(size_t val) noexcept -> size_t{

        for (size_t i = 0; i < sizeof(size_t) * CHAR_BIT; ++i){
            size_t cur = size_t{1u} << i; 

            if (cur >= val){
                return cur;
            }
        }

        return 0u; //
    }

    template <class U, class T>
    constexpr auto operator !=(const dg_raw_ptr<U>& lhs, const dg_raw_ptr<T>& rhs) noexcept -> bool{

        return lhs.get() != rhs.get();
    }

    template <class U>
    constexpr auto operator !=(const dg_raw_ptr<U>& lhs, const std::nullptr_t) noexcept -> bool{

        return lhs != dg_raw_ptr<U>(nullptr);
    }

    template <class T>
    constexpr auto operator !=(const std::nullptr_t, const dg_raw_ptr<T>& rhs) noexcept -> bool{

        return dg_raw_ptr<T>(nullptr) != rhs;
    }

    template <class U, class T>
    constexpr auto operator ==(const dg_raw_ptr<U>& lhs, const dg_raw_ptr<T>& rhs) noexcept -> bool{

        return lhs.get() == rhs.get();
    }

    template <class U>
    constexpr auto operator ==(const dg_raw_ptr<U>& lhs, const std::nullptr_t) noexcept -> bool{

        return lhs == dg_raw_ptr<U>(nullptr);
    }

    template <class T>
    constexpr auto operator ==(const std::nullptr_t, const dg_raw_ptr<T>& rhs) noexcept -> bool{

        return dg_raw_ptr<T>(nullptr) == rhs;
    }

    template <class U, class T>
    constexpr auto operator >(const dg_raw_ptr<U>& lhs, const dg_raw_ptr<T>& rhs) noexcept -> bool{

        return lhs.get() > rhs.get();
    }

    template <class U>
    constexpr auto operator >(const dg_raw_ptr<U>& lhs, const std::nullptr_t) noexcept -> bool{

        return lhs > dg_raw_ptr<U>(nullptr);
    }

    template <class T>
    constexpr auto operator >(const std::nullptr_t, const dg_raw_ptr<T>& rhs) noexcept -> bool{

        return dg_raw_ptr<T>(nullptr) > rhs;
    }

    template <class U, class T>
    constexpr auto operator >=(const dg_raw_ptr<U>& lhs, const dg_raw_ptr<T>& rhs) noexcept -> bool{

        return lhs.get() >= rhs.get();
    }

    template <class U>
    constexpr auto operator >=(const dg_raw_ptr<U>& lhs, const std::nullptr_t) noexcept -> bool{

        return lhs >= dg_raw_ptr<U>(nullptr);
    }

    template <class T>
    constexpr auto operator >=(const std::nullptr_t, const dg_raw_ptr<T>& rhs) noexcept -> bool{

        return dg_raw_ptr<T>(nullptr) >= rhs;
    }

    template <class U, class T>
    constexpr auto operator <(const dg_raw_ptr<U>& lhs, const dg_raw_ptr<T>& rhs) noexcept -> bool{

        return lhs.get() < rhs.get();
    }

    template <class U>
    constexpr auto operator <(const dg_raw_ptr<U>& lhs, const std::nullptr_t) noexcept -> bool{

        return lhs < dg_raw_ptr<U>(nullptr);
    }

    template <class T>
    constexpr auto operator <(const std::nullptr_t, const dg_raw_ptr<T>& rhs) noexcept -> bool{

        return dg_raw_ptr<T>(nullptr) < rhs;
    }   

    template <class U, class T>
    constexpr auto operator <=(const dg_raw_ptr<U>& lhs, const dg_raw_ptr<T>& rhs) noexcept -> bool{

        return lhs.get() <= rhs.get();
    }

    template <class U>
    constexpr auto operator <=(const dg_raw_ptr<U>& lhs, const std::nullptr_t) noexcept -> bool{

        return lhs <= dg_raw_ptr<U>(nullptr);
    }

    template <class T>
    constexpr auto operator <=(const std::nullptr_t, const dg_raw_ptr<T>& rhs) noexcept -> bool{

        return dg_raw_ptr<T>(nullptr) <= rhs;
    }

    template <class U, class T, std::enable_if_t<is_static_ptr_castable_v<U, T> && !std::is_array_v<U> && !std::is_array_v<T>, bool> = true>
    constexpr auto dg_static_raw_pointer_cast(const dg_raw_ptr<T>& ptr) noexcept -> dg_raw_ptr<U>{

        return dg_raw_ptr<U>(ptr.heap_addr(), static_cast<U *>(ptr.get()));
    }

    template <class U, class T, std::enable_if_t<std::is_same_v<U, T> && std::is_array_v<T>, bool> = true>
    constexpr auto dg_static_raw_pointer_cast(const dg_raw_ptr<T>& ptr) noexcept -> dg_raw_ptr<U>{

        return ptr;
    }

    template <class U, class T, std::enable_if_t<is_dynamic_ptr_castable_v<U, T> && !std::is_array_v<U> && !std::is_array_v<T>, bool> = true>
    inline auto dg_dynamic_raw_pointer_cast(const dg_raw_ptr<T>& ptr) noexcept -> dg_raw_ptr<U>{

        return dg_raw_ptr<U>(ptr.heap_addr(), dynamic_cast<U *>(ptr.get()));
    }

    template <class U, class T, std::enable_if_t<std::is_same_v<U, T> && std::is_array_v<T>, bool> = true>
    inline auto dg_dynamic_raw_pointer_cast(const dg_raw_ptr<T>& ptr) noexcept -> dg_raw_ptr<U>{

        return ptr;
    }

    template <class U, class T, std::enable_if_t<is_reinterpret_ptr_castable_v<U, T> && !std::is_array_v<U> && !std::is_array_v<T>, bool> = true>
    inline auto dg_reinterpret_raw_pointer_cast(const dg_raw_ptr<T>& ptr) noexcept -> dg_raw_ptr<U>{

        return dg_raw_ptr<U>(ptr.heap_addr(), reinterpret_cast<U *>(ptr.get()));
    }

    template <class U, class T, std::enable_if_t<std::is_same_v<U, T> && std::is_array_v<T>, bool> = true>
    inline auto dg_reinterpret_raw_pointer_cast(const dg_raw_ptr<T>& ptr) noexcept -> dg_raw_ptr<U>{

        return ptr;
    }

    template <class T, class ...Args, std::enable_if_t<std::is_nothrow_constructible_v<T, Args...> && !std::is_array_v<T>, bool> = true>
    inline auto dg_init(Args&& ...args) -> dg_raw_ptr<T>{
        
        addr_type heap_addr = dg::allocation::cppmalloc(std::integral_constant<size_t, sizeof(T)>{}, std::integral_constant<size_t, alignof(T)>{});
        T * ptr = new (dg::allocation::c_addr(heap_addr)) T(std::forward<Args>(args)...); //optimizable c_addr
        
        return dg_raw_ptr<T>(heap_addr, ptr);
    }

    template <class T, class ...Args, std::enable_if_t<!std::is_nothrow_constructible_v<T, Args...> && std::is_constructible_v<T, Args...> && !std::is_array_v<T>, bool> = true>
    inline auto dg_init(Args&& ...args) -> dg_raw_ptr<T>{

        static int i        = 0;
        addr_type heap_addr = dg::allocation::cppmalloc(std::integral_constant<size_t, sizeof(T)>{}, std::integral_constant<size_t, alignof(T)>{});
        auto deallocator    = [=](int *) noexcept{
            dg::allocation::free(heap_addr);
        };
        auto scope_guard    = std::unique_ptr<int, decltype(deallocator)>(&i, deallocator);
        T * ptr = new (dg::allocation::c_addr(heap_addr)) T(std::forward<Args>(args)...); //optimizable c_addr
        scope_guard.release();

        return dg_raw_ptr<T>(heap_addr, ptr);
    }

    template <class T, std::enable_if_t<std::is_nothrow_default_constructible_v<std::remove_extent_t<T>> && std::is_array_v<T> && !is_bound_array_v<T>, bool> = true>
    inline auto dg_init(const size_t N) -> dg_raw_ptr<T>{
        
        if (N == 0u){
            return nullptr;
        }

        using elem_type             = std::remove_extent_t<T>;
        constexpr size_t header_sz  = get_least_multiple_of_2_eq_exceeds(std::max(sizeof(size_t), alignof(elem_type)));
        const size_t blk_sz         = header_sz + N * sizeof(elem_type); 
        addr_type heap_addr         = dg::allocation::cppmalloc(blk_sz, alignof(elem_type));
        void * cptr                 = dg::allocation::c_addr(heap_addr); //optimizable c_addr
        void * obj_ptr              = static_cast<char *>(cptr) + header_sz; 

        std::memcpy(cptr, &N, sizeof(size_t)); //consider method size() for dg_raw_ptr<T[]> to avoid memory operation
        elem_type * ptr = new (obj_ptr) elem_type[N];

        return dg_raw_ptr<T>(heap_addr, ptr);
    }

    template <class T, std::enable_if_t<!std::is_nothrow_default_constructible_v<std::remove_extent_t<T>> && std::is_default_constructible_v<std::remove_extent_t<T>> && std::is_array_v<T> && !is_bound_array_v<T>, bool> = true>
    inline auto dg_init(const size_t N) -> dg_raw_ptr<T>{

        if (N == 0u){
            return nullptr;
        }

        using elem_type             = std::remove_extent_t<T>;
        static int i                = 0;
        constexpr size_t header_sz  = get_least_multiple_of_2_eq_exceeds(std::max(sizeof(size_t), alignof(elem_type)));
        const size_t blk_sz         = header_sz + N * sizeof(elem_type); 
        addr_type heap_addr         = dg::allocation::cppmalloc(blk_sz, alignof(elem_type));
        auto deallocator            = [=](int *) noexcept{
            dg::allocation::free(heap_addr);
        };
        auto scope_guard            = std::unique_ptr<int, decltype(deallocator)>(&i, deallocator);
        void * cptr                 = dg::allocation::c_addr(heap_addr); //optimizable c_addr
        void * obj_ptr              = static_cast<char *>(cptr) + header_sz; 

        std::memcpy(cptr, &N, sizeof(size_t)); //consider method size() for dg_raw_ptr<T[]> to avoid memory operation
        elem_type * ptr = new (obj_ptr) elem_type[N]; 
        scope_guard.release();

        return dg_raw_ptr<T>(heap_addr, ptr);
    } 

    template <class T, std::enable_if_t<std::is_nothrow_destructible_v<T> && !std::is_array_v<T>, bool> = true>
    inline void dg_del(const dg_raw_ptr<T>& ptr) noexcept{

        if (!ptr){
            return;
        }

        std::destroy_at(ptr.get());
        dg::allocation::free(ptr.heap_addr());
    }

    template <class T, std::enable_if_t<std::is_nothrow_destructible_v<std::remove_extent_t<T>> && std::is_array_v<T>, bool> = true>
    inline void dg_del(const dg_raw_ptr<T>& ptr) noexcept{

        if (!ptr){
            return;
        }

        void * cptr = dg::allocation::c_addr(ptr.heap_addr());
        size_t n    = 0u;

        std::memcpy(&n, cptr, sizeof(size_t));
        std::destroy(ptr.get(), ptr.get() + n);
        dg::allocation::free(ptr.heap_addr());
    }   

    template <class T, class DeleterType>
    static inline constexpr bool meet_deleter_req_v = noexcept(std::declval<const DeleterType&>()(std::declval<const dg_raw_ptr<T>&>())) 
                                                      && std::is_nothrow_destructible_v<DeleterType> 
                                                      && std::is_move_constructible_v<DeleterType> 
                                                      && std::is_move_assignable_v<DeleterType> 
                                                      && std::is_same_v<base_t<DeleterType>, DeleterType>
                                                      && !is_bound_array_v<T>; //

    struct DefaultDeleter{

        constexpr DefaultDeleter() = default;
        constexpr DefaultDeleter(const DefaultDeleter&) = default;
        constexpr DefaultDeleter(DefaultDeleter&&) = default;
        constexpr DefaultDeleter& operator =(const DefaultDeleter&) = default;
        constexpr DefaultDeleter& operator =(DefaultDeleter&&) = default;
        constexpr ~DefaultDeleter() = default; 

        template <class T>
        void operator()(const dg_raw_ptr<T>& arg) const noexcept{

            dg_del(arg);
        }
    };

    template <class T, class DeleterType = DefaultDeleter, std::enable_if_t<meet_deleter_req_v<T, DeleterType>, bool> = true>
    class dg_unique_ptr: private dg_raw_ptr<T>{

        private:

            DeleterType deleter;

            using self              = dg_unique_ptr; 
            using ptr_base          = dg_raw_ptr<T>;  
            using const_ptr_base    = const ptr_base;
            
        public:
            
            template <class _DelType = DeleterType, std::enable_if_t<std::is_default_constructible_v<_DelType>, bool> = true>
            dg_unique_ptr() noexcept(std::is_nothrow_default_constructible_v<_DelType>): ptr_base(), deleter(){}

            template <class _DelType = DeleterType, std::enable_if_t<std::is_default_constructible_v<_DelType>, bool> = true>
            dg_unique_ptr(std::nullptr_t) noexcept(noexcept(dg_unique_ptr())): dg_unique_ptr(){}

            template <class U, class DeleterArg, std::enable_if_t<is_ptr_constructible_fr_v<T, U> && std::is_constructible_v<DeleterType, DeleterArg>, bool> = true>
            explicit dg_unique_ptr(const dg_raw_ptr<U>& ptr, DeleterArg&& deleter) noexcept(std::is_nothrow_constructible_v<DeleterType, DeleterArg>): ptr_base(dg_static_raw_pointer_cast<T>(ptr)), 
                                                                                                                                                       deleter(std::forward<DeleterArg>(deleter)){}
                       
            template <class U, std::enable_if_t<is_strict_ptr_constructible_fr_v<T, U>, bool> = true>
            explicit dg_unique_ptr(dg_unique_ptr<U, DeleterType>&& other) noexcept(std::is_nothrow_move_constructible_v<DeleterType>): ptr_base(dg_static_raw_pointer_cast<T>(other.raw())),
                                                                                                                                       deleter(std::move(other).get_deleter()){
                other.release();
            }

            dg_unique_ptr(self&& other) noexcept(std::is_nothrow_move_constructible_v<DeleterType>): ptr_base(static_cast<ptr_base&&>(other)),
                                                                                                     deleter(std::move(other.deleter)){
                other.release();
            }

            dg_unique_ptr(const self&) = delete;

            ~dg_unique_ptr() noexcept{

                this->free_resource();
            }

            auto operator =(const self&) -> self& = delete;

            inline auto operator =(self&& other) noexcept(std::is_nothrow_move_assignable_v<DeleterType>) -> self&{
                
                if (std::addressof(*this) != std::addressof(other)){ 
                    this->free_resource();
                    static_cast<ptr_base&>(*this) = static_cast<ptr_base&&>(other);
                    this->deleter = std::move(other.deleter);
                    other.release();
                }

                return *this;
            }

            inline void reset() noexcept{
                
                this->free_resource();
                this->release();
            }

            constexpr void release() noexcept{
                
                static_cast<ptr_base&>(*this) = nullptr;
            }

            constexpr auto raw() const noexcept -> const ptr_base&{

                return *this;
            }

            constexpr auto get_deleter() const & noexcept -> const DeleterType&{

                return this->deleter;
            }   

            constexpr auto get_deleter() && noexcept -> DeleterType&&{

                return static_cast<DeleterType&&>(this->deleter);
            }

            using ptr_base::get;

            template <class _ptr_base = const_ptr_base, std::enable_if_t<has_dig_operator_v<_ptr_base> && std::is_same_v<_ptr_base, const_ptr_base>, bool> = true>  
            constexpr auto operator ->() const noexcept(noexcept(ptr_base::operator ->())) -> decltype(auto){
                
                return ptr_base::operator ->();
            }

            template <class _ptr_base = const_ptr_base, std::enable_if_t<has_deref_operator_v<_ptr_base> && std::is_same_v<_ptr_base, const_ptr_base>, bool> = true> 
            constexpr auto operator *() const noexcept(noexcept(ptr_base::operator *())) -> decltype(auto){

                return ptr_base::operator *();
            }

            template <class _ptr_base = const_ptr_base, std::enable_if_t<has_array_deref_operator_v<_ptr_base> && std::is_same_v<_ptr_base, const_ptr_base>, bool> = true> 
            constexpr auto operator [](intmax_t idx) const noexcept(noexcept(ptr_base::operator[](idx))) -> decltype(auto){

                return ptr_base::operator[](idx);
            }

            using ptr_base::operator bool;
                    
        private:

            inline void free_resource() const noexcept{

                if (static_cast<bool>(*this)){
                    this->deleter(this->raw());
                }
            }
    }; 

    template <class T>
    class dg_unique_ptr<T, DefaultDeleter>: private dg_raw_ptr<T>{

        private:

            using self              = dg_unique_ptr; 
            using ptr_base          = dg_raw_ptr<T>;  
            using const_ptr_base    = const ptr_base;
            
        public:

            constexpr dg_unique_ptr() noexcept = default;            

            constexpr dg_unique_ptr(std::nullptr_t) noexcept: dg_unique_ptr(){}

            template <class U, std::enable_if_t<is_ptr_constructible_fr_v<T, U>, bool> = true>
            constexpr explicit dg_unique_ptr(const dg_raw_ptr<U>& ptr) noexcept: ptr_base(dg_static_raw_pointer_cast<T>(ptr)){}
                       
            template <class U, std::enable_if_t<is_strict_ptr_constructible_fr_v<T, U>, bool> = true>
            explicit constexpr dg_unique_ptr(dg_unique_ptr<U, DefaultDeleter>&& other) noexcept: ptr_base(dg_static_raw_pointer_cast<T>(other.raw())){

                other.release();
            }

            constexpr dg_unique_ptr(self&& other) noexcept: ptr_base(static_cast<ptr_base&&>(other)){

                other.release();
            }

            dg_unique_ptr(const self&) = delete;

            ~dg_unique_ptr() noexcept{

                this->free_resource();
            }

            auto operator =(const self&) -> self& = delete;

            inline auto operator =(self&& other) noexcept -> self&{
                
                if (std::addressof(*this) != std::addressof(other)){ 
                    this->free_resource();
                    static_cast<ptr_base&>(*this) = static_cast<ptr_base&&>(other);
                    other.release();
                }

                return *this;
            }

            inline void reset() noexcept{
                
                this->free_resource();
                this->release();
            }

            constexpr void release() noexcept{
                
                static_cast<ptr_base&>(*this) = nullptr;
            }

            constexpr auto raw() const noexcept -> const ptr_base&{

                return *this;
            }

            inline auto get_deleter() const & noexcept -> const DefaultDeleter&{
                
                static auto deleter = DefaultDeleter{};
                return static_cast<const DefaultDeleter&>(deleter);
            }   

            inline auto get_deleter() && noexcept -> DefaultDeleter&&{

                static auto deleter = DefaultDeleter{};
                return static_cast<DefaultDeleter&&>(deleter);
            }

            using ptr_base::get;

            template <class _ptr_base = const_ptr_base, std::enable_if_t<has_dig_operator_v<_ptr_base> && std::is_same_v<_ptr_base, const_ptr_base>, bool> = true>  
            constexpr auto operator ->() const noexcept(noexcept(ptr_base::operator ->())) -> decltype(auto){
                
                return ptr_base::operator ->();
            }

            template <class _ptr_base = const_ptr_base, std::enable_if_t<has_deref_operator_v<_ptr_base> && std::is_same_v<_ptr_base, const_ptr_base>, bool> = true> 
            constexpr auto operator *() const noexcept(noexcept(ptr_base::operator *())) -> decltype(auto){

                return ptr_base::operator *();
            }

            template <class _ptr_base = const_ptr_base, std::enable_if_t<has_array_deref_operator_v<_ptr_base> && std::is_same_v<_ptr_base, const_ptr_base>, bool> = true> 
            constexpr auto operator [](intmax_t idx) const noexcept(noexcept(ptr_base::operator[](idx))) -> decltype(auto){

                return ptr_base::operator[](idx);
            }

            using ptr_base::operator bool;
                    
        private:

            inline void free_resource() const noexcept{

                if (static_cast<bool>(*this)){
                    DefaultDeleter{}(this->raw());
                }
            }
    };

    template <class ...Us, class ...Ts>
    constexpr auto operator !=(const dg_unique_ptr<Us...>& lhs, const dg_unique_ptr<Ts...>& rhs) noexcept -> bool{

        return lhs.raw() != rhs.raw();
    }

    template <class ...Us>
    constexpr auto operator !=(const dg_unique_ptr<Us...>& lhs, const std::nullptr_t) noexcept -> bool{

        return lhs.raw() != nullptr;
    }

    template <class ...Ts>
    constexpr auto operator !=(const std::nullptr_t, const dg_unique_ptr<Ts...>& rhs) noexcept -> bool{

        return nullptr != rhs.raw();
    }

    template <class ...Us, class ...Ts>
    constexpr auto operator ==(const dg_unique_ptr<Us...>& lhs, const dg_unique_ptr<Ts...>& rhs) noexcept -> bool{

        return lhs.raw() == rhs.raw();
    }

    template <class ...Us>
    constexpr auto operator ==(const dg_unique_ptr<Us...>& lhs, const std::nullptr_t) noexcept -> bool{

        return lhs.raw() == nullptr;
    }

    template <class ...Ts>
    constexpr auto operator ==(const std::nullptr_t, const dg_unique_ptr<Ts...>& rhs) noexcept -> bool{

        return nullptr == rhs.raw();
    }

    template <class ...Us, class ...Ts>
    constexpr auto operator >(const dg_unique_ptr<Us...>& lhs, const dg_unique_ptr<Ts...>& rhs) noexcept -> bool{

        return lhs.raw() > rhs.raw();
    }

    template <class ...Us>
    constexpr auto operator >(const dg_unique_ptr<Us...>& lhs, const std::nullptr_t) noexcept -> bool{

        return lhs.raw() > nullptr;
    }

    template <class ...Ts>
    constexpr auto operator >(const std::nullptr_t, const dg_unique_ptr<Ts...>& rhs) noexcept -> bool{

        return nullptr > rhs.raw();
    }

    template <class ...Us, class ...Ts>
    constexpr auto operator >=(const dg_unique_ptr<Us...>& lhs, const dg_unique_ptr<Ts...>& rhs) noexcept -> bool{

        return lhs.raw() >= rhs.raw();
    }

    template <class ...Us>
    constexpr auto operator >=(const dg_unique_ptr<Us...>& lhs, const std::nullptr_t) noexcept -> bool{

        return lhs.raw() >= nullptr;
    }

    template <class ...Ts>
    constexpr auto operator >=(const std::nullptr_t, const dg_unique_ptr<Ts...>& rhs) noexcept -> bool{

        return nullptr >= rhs.raw();
    }

    template <class ...Us, class ...Ts>
    constexpr auto operator <(const dg_unique_ptr<Us...>& lhs, const dg_unique_ptr<Ts...>& rhs) noexcept -> bool{

        return lhs.raw() < rhs.raw();
    }

    template <class ...Us>
    constexpr auto operator <(const dg_unique_ptr<Us...>& lhs, const std::nullptr_t) noexcept -> bool{

        return lhs.raw() < nullptr;
    }

    template <class ...Ts>
    constexpr auto operator <(const std::nullptr_t, const dg_unique_ptr<Ts...>& rhs) noexcept -> bool{

        return nullptr < rhs.raw();
    }   

    template <class ...Us, class ...Ts>
    constexpr auto operator <=(const dg_unique_ptr<Us...>& lhs, const dg_unique_ptr<Ts...>& rhs) noexcept -> bool{

        return lhs.raw() <= rhs.raw();
    }

    template <class ...Us>
    constexpr auto operator <=(const dg_unique_ptr<Us...>& lhs, const std::nullptr_t) noexcept -> bool{

        return lhs.raw() <= nullptr;
    }

    template <class ...Ts>
    constexpr auto operator <=(const std::nullptr_t, const dg_unique_ptr<Ts...>& rhs) noexcept -> bool{

        return nullptr <= rhs.raw();
    }

    template <class T, class U, class DeleterType>
    inline auto dg_static_unique_pointer_cast(dg_unique_ptr<U, DeleterType>&& caster) noexcept(std::is_nothrow_move_constructible_v<DeleterType>){

        auto rs = dg_unique_ptr<T, DeleterType>(dg_static_raw_pointer_cast<T>(caster.raw()), std::move(caster).get_deleter());
        caster.release();

        return rs;
    } 

    template <class T, class U, class DeleterType>
    inline auto dg_dynamic_unique_pointer_cast(dg_unique_ptr<U, DeleterType>&& caster) noexcept(std::is_nothrow_move_constructible_v<DeleterType>){

        //
        auto rs = dg_unique_ptr<T, DeleterType>(dg_dynamic_raw_pointer_cast<T>(caster.raw()), std::move(caster).get_deleter());
        caster.release();
        return rs;
    }

    template <class T, class ...Args>
    inline auto dg_make_unique(Args&& ...args){

        return dg_unique_ptr<T>(dg_init<T>(std::forward<Args>(args)...));
    }

    class VirtualControlBlock{

        public:

            virtual ~VirtualControlBlock() noexcept = default; //bad design - accidential deletion of object could violate atomic contract of functors (when >= two except functors are invoked) - this is precisely the motivation for having del() has a method
    };

    template <class T>
    class control_block{

        private:

            T obj; 
            std::atomic<ref_counter_type> counter;
        
        public:

            template <class ...Args, size_t ...IDX>
            explicit control_block(std::tuple<Args...>&& obj_args,
                                   const std::index_sequence<IDX...>,
                                   ref_counter_type counter) noexcept(std::is_nothrow_constructible_v<T, Args...>): obj(std::get<IDX>(obj_args)...), 
                                                                                                                    counter(counter){}

            template <class ...Args>
            explicit control_block(std::tuple<Args...>&& obj_args, 
                                   ref_counter_type counter) noexcept(noexcept(control_block(std::move(obj_args), std::make_index_sequence<sizeof...(Args)>(), counter))): control_block(std::move(obj_args), std::make_index_sequence<sizeof...(Args)>(), counter){}
            
            constexpr auto get_obj_ptr() noexcept -> T *{

                return &this->obj;
            }

            constexpr auto get_counter_ptr() noexcept -> std::atomic<ref_counter_type> *{

                return &this->counter;
            }
    };

    template <class T, class DeleterType = DefaultDeleter, std::enable_if_t<meet_deleter_req_v<T, DeleterType>, bool> = true>
    class non_contiguous_control_block: public virtual VirtualControlBlock{

        private:

            dg_raw_ptr<T> ptr;
            std::atomic<ref_counter_type> counter;
            DeleterType deleter;

        public:

            template <class DeleterArg>
            explicit non_contiguous_control_block(const dg_raw_ptr<T>& ptr, 
                                                  ref_counter_type counter, 
                                                  DeleterArg&& deleter) noexcept(std::is_nothrow_constructible_v<DeleterType, DeleterArg>): ptr(ptr), 
                                                                                                                                            counter(counter), 
                                                                                                                                            deleter(std::forward<DeleterArg>(deleter)){}
            ~non_contiguous_control_block() noexcept{

                this->deleter(this->ptr);
            }

            constexpr auto get_counter_ptr() noexcept -> std::atomic<ref_counter_type> *{

                return &this->counter;
            }
    };

    template <class T>
    class non_contiguous_control_block<T, DefaultDeleter>: public virtual VirtualControlBlock{

        private:

            dg_raw_ptr<T> ptr;
            std::atomic<ref_counter_type> counter;
        
        public:

            constexpr explicit non_contiguous_control_block(const dg_raw_ptr<T>& ptr, 
                                                            ref_counter_type counter,
                                                            const DefaultDeleter) noexcept: ptr(ptr), 
                                                                                            counter(counter){}

            ~non_contiguous_control_block() noexcept{

                DefaultDeleter{}(this->ptr);
            }

            constexpr auto get_counter_ptr() noexcept -> std::atomic<ref_counter_type> *{

                return &this->counter;
            }
    };

    template <class T>
    class contiguous_control_block: public virtual VirtualControlBlock, 
                                    private control_block<T>{
        
        private:

            using base = control_block<T>;

        public:

            template <class ...Args>
            explicit contiguous_control_block(std::tuple<Args...>&& obj_args, 
                                              ref_counter_type counter) noexcept(std::is_nothrow_constructible_v<base, std::tuple<Args...>&&, ref_counter_type>): base(std::move(obj_args), counter){}

            using base::get_obj_ptr;
            using base::get_counter_ptr;
    };

    struct inc_ref_on_init_tag{
        constexpr explicit inc_ref_on_init_tag() noexcept = default;
    }; 

    template <class T>
    class dg_shared_ptr_base: private dg_raw_ptr<T>{

        private:

            dg_raw_ptr<VirtualControlBlock> ctrl_blk;
            dg_raw_ptr<std::atomic<ref_counter_type>> counter; //decomposition by private inheritance to avoid memory dereferencing (future consideration)

            using self              = dg_shared_ptr_base;
            using ptr_base          = dg_raw_ptr<T>; 
            using const_ptr_base    = const ptr_base;
        
        public:

            constexpr dg_shared_ptr_base() noexcept = default;

            constexpr explicit dg_shared_ptr_base(const dg_raw_ptr<VirtualControlBlock>& ctrl_blk,
                                                  const dg_raw_ptr<std::atomic<ref_counter_type>>& counter,
                                                  const dg_raw_ptr<T>& ptr) noexcept: ctrl_blk(ctrl_blk),
                                                                                      counter(counter),
                                                                                      ptr_base(ptr){}

            explicit dg_shared_ptr_base(const dg_raw_ptr<VirtualControlBlock>& ctrl_blk, 
                                        const dg_raw_ptr<std::atomic<ref_counter_type>>& counter, 
                                        const dg_raw_ptr<T>& ptr, 
                                        const inc_ref_on_init_tag) noexcept: dg_shared_ptr_base(ctrl_blk, counter, ptr){
                
                this->inc_ref();
            } 

            dg_shared_ptr_base(const self& other) noexcept: dg_shared_ptr_base(other.ctrl_blk, other.counter, static_cast<const ptr_base&>(other)){

                this->inc_ref();
            }
            
            constexpr dg_shared_ptr_base(self&& other) noexcept: ctrl_blk(std::move(other.ctrl_blk)),
                                                                 counter(std::move(other.counter)),
                                                                 ptr_base(static_cast<ptr_base&&>(other)){
                other.release();
            }

            ~dg_shared_ptr_base() noexcept{

                this->dec_ref();
            }

            inline auto operator =(const self& other) noexcept -> self&{
                
                if (std::addressof(*this) != std::addressof(other)){
                    this->dec_ref();
                    this->ctrl_blk  = other.ctrl_blk;
                    this->counter   = other.counter;
                    static_cast<ptr_base&>(*this) = static_cast<const ptr_base&>(other);
                    this->inc_ref();
                }

                return *this;
            }

            inline auto operator =(self&& other) noexcept -> self&{

                if (std::addressof(*this) != std::addressof(other)){
                    this->dec_ref();
                    this->ctrl_blk  = std::move(other.ctrl_blk);
                    this->counter   = std::move(other.counter);
                    static_cast<ptr_base&>(*this) = static_cast<ptr_base&&>(other);      
                    other.release();
                }

                return *this;
            }

            constexpr auto raw() const noexcept -> const ptr_base&{

                return *this;
            }

            constexpr auto get_ctrl_blk() const noexcept -> const dg_raw_ptr<VirtualControlBlock>&{

                return this->ctrl_blk;
            } 

            constexpr auto get_counter() const noexcept -> const dg_raw_ptr<std::atomic<ref_counter_type>>&{

                return this->counter;
            } 

            inline void reset() noexcept{

                this->dec_ref();
                this->release();
            }

            using ptr_base::get;

            template <class _ptr_base = const_ptr_base, std::enable_if_t<has_dig_operator_v<_ptr_base> && std::is_same_v<_ptr_base, const_ptr_base>, bool> = true> 
            constexpr auto operator ->() const noexcept(noexcept(ptr_base::operator ->())) -> decltype(auto){
                
                return ptr_base::operator ->();
            }

            template <class _ptr_base = const_ptr_base, std::enable_if_t<has_deref_operator_v<_ptr_base> && std::is_same_v<_ptr_base, const_ptr_base>, bool> = true> 
            constexpr auto operator *() const noexcept(noexcept(ptr_base::operator *())) -> decltype(auto){

                return ptr_base::operator *();
            }

            template <class _ptr_base = const_ptr_base, std::enable_if_t<has_array_deref_operator_v<_ptr_base> && std::is_same_v<_ptr_base, const_ptr_base>, bool> = true> 
            constexpr auto operator [](intmax_t idx) const noexcept(noexcept(ptr_base::operator[](idx))) -> decltype(auto){

                return ptr_base::operator[](idx);
            }

            using ptr_base::operator bool;

        private:

            constexpr void release() noexcept{

                this->ctrl_blk  = nullptr;
                this->counter   = nullptr;
                static_cast<ptr_base&>(*this) = nullptr;
            } 

            inline void inc_ref() const noexcept{
                
                if (static_cast<bool>(*this)){
                    this->counter->fetch_add(1u, std::memory_order_relaxed);
                }
            }

            inline void dec_ref() const noexcept{
                
                if (static_cast<bool>(*this) && this->counter->fetch_sub(1u, std::memory_order_acq_rel) == 1u){
                    dg_del(this->ctrl_blk);
                }
            }
    };
    
    struct base_init_tag{
        constexpr explicit base_init_tag() noexcept = default;
    };

    template <class T>
    class dg_shared_ptr: private dg_shared_ptr_base<T>{

        private:

            using self          = dg_shared_ptr;
            using base          = dg_shared_ptr_base<T>; 
            using const_base    = const base;

            explicit dg_shared_ptr(const base_init_tag, const base& other): base(other){}
            constexpr explicit dg_shared_ptr(const base_init_tag, base&& other): base(static_cast<base&&>(other)){}

        public:

            static_assert(!is_bound_array_v<T>); //move precond to header next iteration - sfinae

            constexpr dg_shared_ptr() noexcept = default;

            constexpr dg_shared_ptr(std::nullptr_t) noexcept: dg_shared_ptr(){}

            template <class U, class ...Args, std::enable_if_t<is_ptr_constructible_fr_v<T, U>, bool> = true>
            explicit dg_shared_ptr(dg_unique_ptr<U, Args...>&& obj) noexcept(noexcept(dg_shared_ptr(obj.raw(), std::move(obj).get_deleter()))): dg_shared_ptr(obj.raw(), std::move(obj).get_deleter()){

                obj.release();
            }

            template <class U, std::enable_if_t<is_strict_ptr_constructible_fr_v<T, U>, bool> = true>
            dg_shared_ptr(const dg_shared_ptr<U>& obj) noexcept: base(obj.get_deleter(), obj.get_counter(), dg_static_raw_pointer_cast<T>(obj.raw()), inc_ref_on_init_tag()){}
            
            template <class U, std::enable_if_t<is_strict_ptr_constructible_fr_v<T, U>, bool> = true>
            constexpr dg_shared_ptr(dg_shared_ptr<U>&& obj) noexcept: base(obj.get_deleter(), obj.get_counter(), dg_static_raw_pointer_cast<T>(obj.raw())){

                obj.release();
            }

            template <class U, class DeleterArg = DefaultDeleter, std::enable_if_t<is_ptr_constructible_fr_v<T, U> && meet_deleter_req_v<U, base_t<DeleterArg>> && std::is_constructible_v<base_t<DeleterArg>, DeleterArg>, bool> = true>
            explicit dg_shared_ptr(const dg_raw_ptr<U>& ptr, DeleterArg&& deleter = DeleterArg{}): base(){
                
                using deleter_type          = base_t<DeleterArg>;
                auto ctrl_blk               = dg_init<non_contiguous_control_block<T, deleter_type>>(dg_static_pointer_cast<T>(ptr), 1u, std::forward<DeleterArg>(deleter));
                auto casted_ctrl_blk        = dg_static_pointer_cast<VirtualControlBlock>(ctrl_blk);
                static_cast<base&>(*this)   = base(casted_ctrl_blk, dg_raw_ptr<std::atomic<ref_counter_type>>(ctrl_blk.heap_addr(), ctrl_blk->get_counter_ptr()), dg_static_pointer_cast<T>(ptr));
            }
            
            dg_shared_ptr(const self& other) noexcept = default;
            constexpr dg_shared_ptr(self&& other) noexcept = default;
            ~dg_shared_ptr() noexcept = default;
            auto operator =(const self& other) noexcept -> self& = default; 
            auto operator =(self&& other) noexcept -> self& = default;

            using base::raw;
            using base::get;
            using base::reset;
            using base::operator bool;

            template <class _ptr_base = const_base, std::enable_if_t<has_dig_operator_v<_ptr_base> && std::is_same_v<_ptr_base, const_base>, bool> = true> 
            constexpr auto operator ->() const noexcept(noexcept(base::operator ->())) -> decltype(auto){
                
                return base::operator ->();
            }

            template <class _ptr_base = const_base, std::enable_if_t<has_deref_operator_v<_ptr_base> && std::is_same_v<_ptr_base, const_base>, bool> = true> 
            constexpr auto operator *() const noexcept(noexcept(base::operator *())) -> decltype(auto){

                return base::operator *();
            }

            template <class _ptr_base = const_base, std::enable_if_t<has_array_deref_operator_v<_ptr_base> && std::is_same_v<_ptr_base, const_base>, bool> = true> 
            constexpr auto operator [](intmax_t idx) const noexcept(noexcept(base::operator[](idx))) -> decltype(auto){

                return base::operator[](idx);
            }

            template <class To, class Fr>
            friend auto dg_static_shared_pointer_cast(const dg_shared_ptr<Fr>&) noexcept;

            template <class To, class Fr>
            friend auto dg_static_shared_pointer_cast(dg_shared_ptr<Fr>&&) noexcept;

            template <class To, class Fr>
            friend auto dg_dynamic_shared_pointer_cast(const dg_shared_ptr<Fr>&) noexcept;

            template <class To, class Fr>
            friend auto dg_dynamic_shared_pointer_cast(dg_shared_ptr<Fr>&&) noexcept;

            template <class To, class Fr>
            friend auto dg_reinterpret_shared_pointer_cast(const dg_shared_ptr<Fr>&) noexcept;

            template <class To, class Fr>
            friend auto dg_reinterpret_shared_pointer_cast(dg_shared_ptr<Fr>&&) noexcept;

            template <class Arg, class ...Args>
            friend auto dg_make_shared(Args&&...);

            template <class U>
            friend class dg_shared_ptr;
    };

    template <class U, class T>
    constexpr auto operator !=(const dg_shared_ptr<U>& lhs, const dg_shared_ptr<T>& rhs) noexcept -> bool{

        return lhs.raw() != rhs.raw();
    }

    template <class U>
    constexpr auto operator !=(const dg_shared_ptr<U>& lhs, const std::nullptr_t) noexcept -> bool{

        return lhs.raw() != nullptr;
    }

    template <class T>
    constexpr auto operator !=(const std::nullptr_t, const dg_shared_ptr<T>& rhs) noexcept -> bool{

        return nullptr != rhs.raw();
    }

    template <class U, class T>
    constexpr auto operator ==(const dg_shared_ptr<U>& lhs, const dg_shared_ptr<T>& rhs) noexcept -> bool{

        return lhs.raw() == rhs.raw();
    }

    template <class U>
    constexpr auto operator ==(const dg_shared_ptr<U>& lhs, const std::nullptr_t) noexcept -> bool{

        return lhs.raw() == nullptr;
    }

    template <class T>
    constexpr auto operator ==(const std::nullptr_t, const dg_shared_ptr<T>& rhs) noexcept -> bool{

        return nullptr == rhs.raw();
    }

    template <class U, class T>
    constexpr auto operator >(const dg_shared_ptr<U>& lhs, const dg_shared_ptr<T>& rhs) noexcept -> bool{

        return lhs.raw() > rhs.raw();
    }

    template <class U>
    constexpr auto operator >(const dg_shared_ptr<U>& lhs, const std::nullptr_t) noexcept -> bool{

        return lhs.raw() > nullptr;
    }

    template <class T>
    constexpr auto operator >(const std::nullptr_t, const dg_shared_ptr<T>& rhs) noexcept -> bool{

        return nullptr > rhs.raw();
    }

    template <class U, class T>
    constexpr auto operator >=(const dg_shared_ptr<U>& lhs, const dg_shared_ptr<T>& rhs) noexcept -> bool{

        return lhs.raw() >= rhs.raw();
    }

    template <class U>
    constexpr auto operator >=(const dg_shared_ptr<U>& lhs, const std::nullptr_t) noexcept -> bool{

        return lhs.raw() >= nullptr;
    }

    template <class T>
    constexpr auto operator >=(const std::nullptr_t, const dg_shared_ptr<T>& rhs) noexcept -> bool{

        return nullptr >= rhs.raw();
    }

    template <class U, class T>
    constexpr auto operator <(const dg_shared_ptr<U>& lhs, const dg_shared_ptr<T>& rhs) noexcept -> bool{

        return lhs.raw() < rhs.raw();
    }

    template <class U>
    constexpr auto operator <(const dg_shared_ptr<U>& lhs, const std::nullptr_t) noexcept -> bool{

        return lhs.raw() < nullptr;
    }

    template <class T>
    constexpr auto operator <(const std::nullptr_t, const dg_shared_ptr<T>& rhs) noexcept -> bool{

        return nullptr < rhs.raw();
    }   

    template <class U, class T>
    constexpr auto operator <=(const dg_shared_ptr<U>& lhs, const dg_shared_ptr<T>& rhs) noexcept -> bool{

        return lhs.raw() <= rhs.raw();
    }

    template <class U>
    constexpr auto operator <=(const dg_shared_ptr<U>& lhs, const std::nullptr_t) noexcept -> bool{

        return lhs.raw() <= nullptr;
    }

    template <class T>
    constexpr auto operator <=(const std::nullptr_t, const dg_shared_ptr<T>& rhs) noexcept -> bool{

        return nullptr <= rhs.raw();
    }

    template <class T, class U>
    inline auto dg_static_shared_pointer_cast(const dg_shared_ptr<U>& caster) noexcept{

        return dg_shared_ptr<T>(base_init_tag(), dg_shared_ptr_base<T>(caster.get_deleter(), caster.get_counter(), dg_static_raw_pointer_cast<T>(caster.raw()), inc_ref_on_init_tag()));
    } 

    template <class T, class U>
    inline auto dg_static_shared_pointer_cast(dg_shared_ptr<U>&& caster) noexcept{ 

        dg_shared_ptr<T> rs(base_init_tag(), dg_shared_ptr_base<T>(caster.get_deleter(), caster.get_counter(), dg_static_raw_pointer_cast<T>(caster.raw())));
        caster.release();

        return rs;
    }

    template <class T, class U>
    inline auto dg_dynamic_shared_pointer_cast(const dg_shared_ptr<U>& caster) noexcept{

        if (auto castee = dg_dynamic_raw_pointer_cast<T>(caster.raw()); castee){
            return dg_shared_ptr<T>(base_init_tag(), dg_shared_ptr_base<T>(caster.get_deleter(), caster.get_counter(), castee, inc_ref_on_init_tag()));
        }
        return dg_shared_ptr<T>();
    }

    template <class T, class U>
    inline auto dg_reinterpret_shared_pointer_cast(const dg_shared_ptr<U>& caster) noexcept{

        return dg_shared_ptr<T>(base_init_tag(), dg_shared_ptr_base<T>(caster.get_deleter(), caster.get_counter(), dg_reinterpret_raw_pointer_cast<T>(caster.raw()), inc_ref_on_init_tag()));
    }

    template <class T, class U>
    inline auto dg_reinterpret_shared_pointer_cast(dg_shared_ptr<U>&& caster) noexcept{

        dg_shared_ptr<T> rs(base_init_tag(), dg_shared_ptr_base<T>(caster.get_deleter(), caster.get_counter(), dg_reinterpret_raw_pointer_cast<T>(caster.raw())));
        caster.release();

        return rs;
    }

    template <class T, class ...Args, std::enable_if_t<!std::is_array_v<T>, bool> = true>
    inline auto dg_make_shared(Args&& ...args){

        auto ctrl_blk   = dg_init<contiguous_control_block<T>>(std::forward_as_tuple(std::forward<Args>(args)...), 1u); 
        auto rs         = dg_shared_ptr<T>(base_init_tag{}, dg_shared_ptr_base<T>(dg_static_pointer_cast<VirtualControlBlock>(ctrl_blk), dg_raw_ptr<std::atomic<ref_counter_type>>(ctrl_blk.heap_addr(), ctrl_blk->get_counter_ptr()), dg_raw_ptr<T>(ctrl_blk.heap_addr(), ctrl_blk->get_obj_ptr())));

        return rs;
    }

    template <class T, class ...Args, std::enable_if_t<std::is_array_v<T>, bool> = true>
    inline auto dg_make_shared(Args&& ...args){

        return dg_shared_ptr<T>(dg_make_unique<T>(std::forward<Args>(args)...));
    }

    template <class T>
    class dg_simple_shared_ptr: private dg_raw_ptr<control_block<T>>{

        private:    

            using self = dg_simple_shared_ptr;
            using base = dg_raw_ptr<control_block<T>>;

            constexpr explicit dg_simple_shared_ptr(const base_init_tag, const dg_raw_ptr<control_block<T>>& other) noexcept: base(other){}

        public:

            static_assert(meet_deleter_req_v<T, DefaultDeleter> && !std::is_array_v<T>); //sfinae - move to header next iteration

            constexpr dg_simple_shared_ptr() noexcept = default;
            
            constexpr dg_simple_shared_ptr(std::nullptr_t) noexcept: dg_simple_shared_ptr(){}

            dg_simple_shared_ptr(const self& other) noexcept: base(static_cast<const base&>(other)){
                
                this->inc_ref();
            } 

            constexpr dg_simple_shared_ptr(self&& other) noexcept: base(static_cast<base&&>(other)){
                
                other.release();
            }

            ~dg_simple_shared_ptr() noexcept{

                this->dec_ref();
            }

            inline auto operator =(const self& other) noexcept -> self&{

                if (std::addressof(*this) != std::addressof(other)){
                    this->dec_ref();
                    static_cast<base&>(*this) = static_cast<const base&>(other);
                    this->inc_ref();
                }

                return *this;
            }

            inline auto operator =(self&& other) noexcept -> self&{

                if (std::addressof(*this) != std::addressof(other)){
                    this->dec_ref();
                    static_cast<base&>(*this) = static_cast<base&&>(other);
                    other.release();
                }

                return *this;
            }
            
            using base::operator bool;

            inline void reset() noexcept{

                this->dec_ref();
                this->release();
            }

            constexpr auto raw() const noexcept -> const dg_raw_ptr<control_block<T>>&{ //encapsulation - next iteration

                return *this;
            }
            
            inline auto get() const noexcept -> T *{

                return std::launder(reinterpret_cast<T *>(base::get()));
            } 

            inline auto operator ->() const noexcept -> T *{

                return std::launder(reinterpret_cast<T *>(base::operator ->()));
            }

            inline auto operator *() const noexcept -> T&{

                return *std::launder(reinterpret_cast<T *>(&(base::operator *())));
            }

            template <class U, class ...Args>
            friend auto dg_make_simple_shared(Args&&...);
            
        private:

            constexpr void release() noexcept{

                static_cast<base&>(*this) = nullptr;
            }

            inline void dec_ref() const noexcept{

                if (static_cast<bool>(*this) && base::get()->get_counter_ptr()->fetch_sub(1u, std::memory_order_acq_rel) == 1u){
                    DefaultDeleter{}(static_cast<const base&>(*this));
                }
            }

            inline void inc_ref() const noexcept{

                if (static_cast<bool>(*this)){
                    base::get()->get_counter_ptr()->fetch_add(1u, std::memory_order_relaxed);
                }
            }
    };

    template <class U, class T>
    constexpr auto operator !=(const dg_simple_shared_ptr<U>& lhs, const dg_simple_shared_ptr<T>& rhs) noexcept -> bool{

        return lhs.raw() != rhs.raw();
    }

    template <class U>
    constexpr auto operator !=(const dg_simple_shared_ptr<U>& lhs, const std::nullptr_t) noexcept -> bool{

        return lhs.raw() != nullptr;
    }

    template <class T>
    constexpr auto operator !=(const std::nullptr_t, const dg_simple_shared_ptr<T>& rhs) noexcept -> bool{

        return nullptr != rhs.raw();
    }

    template <class U, class T>
    constexpr auto operator ==(const dg_simple_shared_ptr<U>& lhs, const dg_simple_shared_ptr<T>& rhs) noexcept -> bool{

        return lhs.raw() == rhs.raw();
    }

    template <class U>
    constexpr auto operator ==(const dg_simple_shared_ptr<U>& lhs, const std::nullptr_t) noexcept -> bool{

        return lhs.raw() == nullptr;
    }

    template <class T>
    constexpr auto operator ==(const std::nullptr_t, const dg_simple_shared_ptr<T>& rhs) noexcept -> bool{

        return nullptr == rhs.raw();
    }

    template <class U, class T>
    constexpr auto operator >(const dg_simple_shared_ptr<U>& lhs, const dg_simple_shared_ptr<T>& rhs) noexcept -> bool{

        return lhs.raw() > rhs.raw();
    }

    template <class U>
    constexpr auto operator >(const dg_simple_shared_ptr<U>& lhs, const std::nullptr_t) noexcept -> bool{

        return lhs.raw() > nullptr;
    }

    template <class T>
    constexpr auto operator >(const std::nullptr_t, const dg_simple_shared_ptr<T>& rhs) noexcept -> bool{

        return nullptr > rhs.raw();
    }

    template <class U, class T>
    constexpr auto operator >=(const dg_simple_shared_ptr<U>& lhs, const dg_simple_shared_ptr<T>& rhs) noexcept -> bool{

        return lhs.raw() >= rhs.raw();
    }

    template <class U>
    constexpr auto operator >=(const dg_simple_shared_ptr<U>& lhs, const std::nullptr_t) noexcept -> bool{

        return lhs.raw() >= nullptr;
    }

    template <class T>
    constexpr auto operator >=(const std::nullptr_t, const dg_simple_shared_ptr<T>& rhs) noexcept -> bool{

        return nullptr >= rhs.raw();
    }

    template <class U, class T>
    constexpr auto operator <(const dg_simple_shared_ptr<U>& lhs, const dg_simple_shared_ptr<T>& rhs) noexcept -> bool{

        return lhs.raw() < rhs.raw();
    }

    template <class U>
    constexpr auto operator <(const dg_simple_shared_ptr<U>& lhs, const std::nullptr_t) noexcept -> bool{

        return lhs.raw() < nullptr;
    }

    template <class T>
    constexpr auto operator <(const std::nullptr_t, const dg_simple_shared_ptr<T>& rhs) noexcept -> bool{

        return nullptr < rhs.raw();
    }   

    template <class U, class T>
    constexpr auto operator <=(const dg_simple_shared_ptr<U>& lhs, const dg_simple_shared_ptr<T>& rhs) noexcept -> bool{

        return lhs.raw() <= rhs.raw();
    }

    template <class U>
    constexpr auto operator <=(const dg_simple_shared_ptr<U>& lhs, const std::nullptr_t) noexcept -> bool{

        return lhs.raw() <= nullptr;
    }

    template <class T>
    constexpr auto operator <=(const std::nullptr_t, const dg_simple_shared_ptr<T>& rhs) noexcept -> bool{

        return nullptr <= rhs.raw();
    }

    template <class T, class ...Args>
    inline auto dg_make_simple_shared(Args&& ...args){

        static_assert(!std::is_array_v<T>); //weird
        return dg_simple_shared_ptr<T>(base_init_tag{}, dg_init<control_block<T>>(std::forward_as_tuple(std::forward<Args>(args)...), 1u));
    }

    template <class T>
    struct is_dg_raw_ptr: std::false_type{};

    template <class ...Args>
    struct is_dg_raw_ptr<dg_raw_ptr<Args...>>: std::true_type{};

    template <class T>
    struct is_dg_unique_ptr: std::false_type{};

    template <class ...Args>
    struct is_dg_unique_ptr<dg_unique_ptr<Args...>>: std::true_type{};

    template <class T>
    struct is_dg_shared_ptr: std::false_type{};

    template <class ...Args>
    struct is_dg_shared_ptr<dg_shared_ptr<Args...>>: std::true_type{};

    template <class T>
    static inline constexpr bool is_dg_raw_ptr_v    = is_dg_raw_ptr<T>::value;

    template <class T>
    static inline constexpr bool is_dg_unique_ptr_v = is_dg_unique_ptr<T>::value;

    template <class T>
    static inline constexpr bool is_dg_shared_ptr_v = is_dg_shared_ptr<T>::value;

    template <class U, class T>
    inline auto dg_static_pointer_cast(T&& caster){

        using caster_t = base_t<T>;

        if constexpr(is_dg_raw_ptr_v<caster_t>){
            return dg_static_raw_pointer_cast<U>(std::forward<T>(caster));
        } else if constexpr(is_dg_unique_ptr_v<caster_t>){
            return dg_static_unique_pointer_cast<U>(std::forward<T>(caster));
        } else if constexpr(is_dg_shared_ptr_v<caster_t>){
            return dg_static_shared_pointer_cast<U>(std::forward<T>(caster));
        } else{
            static_assert(FALSE_VAL<>, "unreachable");
        }
    } 

    template <class U, class T>
    inline auto dg_reinterpret_pointer_cast(T&& caster){

        using caster_t = base_t<T>;

        if constexpr(is_dg_raw_ptr_v<caster_t>){
            return dg_reinterpret_raw_pointer_cast<U>(std::forward<T>(caster));
        } else if constexpr(is_dg_unique_ptr_v<caster_t>){
            return dg_reinterpret_unique_pointer_cast<U>(std::forward<T>(caster));
        } else if constexpr(is_dg_shared_ptr_v<caster_t>){
            return dg_reinterpret_shared_pointer_cast<U>(std::forward<T>(caster));
        } else{
            static_assert(FALSE_VAL<>, "unreachable");
        }
    }

    template <class U, class T>
    inline auto dg_dynamic_pointer_cast(T&& caster){

        using caster_t = base_t<T>;

        if constexpr(is_dg_raw_ptr_v<caster_t>){
            return dg_dynamic_raw_pointer_cast<U>(std::forward<T>(caster));
        } else if constexpr(is_dg_unique_ptr_v<caster_t>){
            return dg_dynamic_unique_pointer_cast<U>(std::forward<T>(caster));
        } else if constexpr(is_dg_shared_ptr_v<caster_t>){
            return dg_dynamic_shared_pointer_cast<U>(std::forward<T>(caster));
        } else{
            static_assert(FALSE_VAL<>, "unreachable");
        }
    }
}

#endif