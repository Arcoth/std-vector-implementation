/* Copyright (c) 2016, Robert Haberlach
   All rights reserved. Distributed under the BSD 2-clause license (see LICENSE.txt). */

#include <algorithm>
#include <iterator>
#include <tuple>
#include <type_traits>

// define as constexpr once literal types can have destructors
#define CONSTEXPR

namespace detail {
	template <typename I, typename C>
	constexpr bool hasCategory = std::is_base_of_v<C,
		  typename std::iterator_traits<I>::iterator_category>;

	template <typename InputIterator, typename Sentinel>
	constexpr auto distance(InputIterator begin, Sentinel end) {
		if constexpr(hasCategory<InputIterator, std::random_access_iterator_tag>)
			return end - begin;
		// Don't put into an else (constexpr) so we force the return type accordingly
		typename std::iterator_traits<InputIterator>::difference_type d = 0;
		while (begin != end)
			++begin, ++d;
		return d;
	}

	template <typename F>
	struct OnDestruction {
		F f;
		CONSTEXPR OnDestruction(F const& f) : f(f) {}
		CONSTEXPR ~OnDestruction() {f();}
	};
}

template <typename T, typename Alloc = std::allocator<T>>
class vector {
	using _alloc_traits = typename std::allocator_traits<Alloc>::template rebind_traits<T>;
public:
	using allocator_type  = typename _alloc_traits::allocator_type;
	using value_type      = typename _alloc_traits::value_type;
	using size_type       = typename _alloc_traits::size_type;
	using difference_type = typename _alloc_traits::difference_type;
	using pointer         = typename _alloc_traits::pointer;
	using const_pointer   = typename _alloc_traits::const_pointer;
	using reference       = value_type&;
	using const_reference = value_type const&;

	using iterator = pointer;
	using const_iterator = const_pointer;

	// For constexpr, can also use my implementation in Constainer
	// https://github.com/Arcoth/Constainer/blob/master/impl/Iterator/ReverseIterator.hxx
	using reverse_iterator = std::reverse_iterator<iterator>;
	using const_reverse_iterator = std::reverse_iterator<const_iterator>;

private:
	std::tuple<allocator_type, pointer, pointer, pointer> _data;
	CONSTEXPR auto& _begin()       {return std::get<1>(_data);}
	CONSTEXPR auto& _begin() const {return std::get<1>(_data);}
	CONSTEXPR auto& _end()       {return std::get<2>(_data);}
	CONSTEXPR auto& _end() const {return std::get<2>(_data);}
	CONSTEXPR auto& _storage_end()       {return std::get<3>(_data);}
	CONSTEXPR auto& _storage_end() const {return std::get<3>(_data);}
	CONSTEXPR auto& _alloc()     {return std::get<0>(_data);}
	CONSTEXPR auto& _alloc() const {return std::get<0>(_data);}

	CONSTEXPR void _drop_ownership() noexcept {
		_begin() = {};
		_end() = {};
		_storage_end() = {};
	}

	template <typename I>
	static constexpr bool isMultiPass = detail::hasCategory<I, std::forward_iterator_tag>;

public:
	CONSTEXPR       iterator  begin()       {return _begin();}
	CONSTEXPR const_iterator  begin() const {return _begin();}
	CONSTEXPR const_iterator cbegin() const {return _begin();}
	CONSTEXPR       iterator    end()       {return _end();}
	CONSTEXPR const_iterator    end() const {return _end();}
	CONSTEXPR const_iterator   cend() const {return _end();}
	CONSTEXPR       reverse_iterator  rbegin()       {return std::make_reverse_iterator(end());}
	CONSTEXPR const_reverse_iterator  rbegin() const {return std::make_reverse_iterator(end());}
	CONSTEXPR const_reverse_iterator crbegin() const {return std::make_reverse_iterator(end());}
	CONSTEXPR       reverse_iterator    rend()       {return std::make_reverse_iterator(begin());}
	CONSTEXPR const_reverse_iterator    rend() const {return std::make_reverse_iterator(begin());}
	CONSTEXPR const_reverse_iterator   crend() const {return std::make_reverse_iterator(begin());}
	CONSTEXPR allocator_type get_allocator() const noexcept {return _alloc();}

	// tuple's default ctor value-initializes the pointers, i.e. they will compare equal and zero
	CONSTEXPR vector() = default;
	explicit CONSTEXPR vector(allocator_type const& alloc) noexcept
	  : _data(alloc, nullptr, nullptr, nullptr) {}

	explicit CONSTEXPR vector(size_type count) {
        resize(count);}
	CONSTEXPR vector(size_type count, const_reference val, allocator_type const& alloc = {})
	  : vector(alloc) {
		assign(count, val); }

	/* Requiring identical types for begin and end "iterator"s is needlessly restrictive.
	   The Ranges Technical Specification incorporates this fix. */
	template <typename InputIterator, typename Sentinel,
	          typename = std::enable_if_t<detail::hasCategory<InputIterator, std::input_iterator_tag>>>
	CONSTEXPR vector(InputIterator first, Sentinel last) {
		assign(first, last); }
	template <typename InputIterator, typename Sentinel,
	          typename = std::enable_if_t<detail::hasCategory<InputIterator, std::input_iterator_tag>>>
	CONSTEXPR vector(InputIterator first, Sentinel last, allocator_type const& alloc)
	  : vector(alloc) {
		assign(first, last); }
	CONSTEXPR vector(std::initializer_list<value_type> ilist,
	                        allocator_type const& alloc = {}) : vector(ilist.begin(), ilist.end(), alloc) {}

	// For allocator behavior and exception specification, see [container.requirements.general]/8
	CONSTEXPR vector(vector const& other)
	  : vector(other.begin(), other.end(), _alloc_traits::select_on_container_copy_construction(other._alloc())) {}
	CONSTEXPR vector(vector const& other, allocator_type const& alloc)
	  : vector(other.begin(), other.end(), _alloc) {}
	CONSTEXPR vector(vector && other) noexcept
	  : _data(other._begin(), other._end(), std::move(other._alloc())) {
		other._drop_ownership();}
	CONSTEXPR vector(vector && other, allocator_type const& alloc) noexcept
	  : _data(other._begin(), other._end(), alloc) {
		other._drop_ownership();}

	CONSTEXPR vector& operator=(vector const& other) {
		if constexpr(_alloc_traits::propagate_on_container_copy_assignment::value) {
			if (not _alloc_traits::is_always_equal::value && _alloc() != other._alloc())
				_destroy();
			_alloc() = other._alloc();
		}
		assign(other.begin(), other.end());
		return *this;
	}

	/* Note: we adopt libstdc++'s extension, that if
	   _alloc_traits::propagate_on_container_move_assignment || _alloc_traits::is_always_equal,
	   the element types need not be MoveAssignable or MoveInsertable */
	CONSTEXPR vector& operator=(vector&& other)
	   noexcept(_alloc_traits::propagate_on_container_move_assignment::value
	         or _alloc_traits::is_always_equal::value) {
		if constexpr(_alloc_traits::propagate_on_container_move_assignment::value)
			_alloc() = std::move(other._alloc());
		if constexpr(_alloc_traits::propagate_on_container_move_assignment::value or _alloc_traits::is_always_equal::value) {
			_begin()       = other._begin();
			_end()         = other._end()  ;
			_storage_end() = other._storage_end();
			other._drop_ownership();
		}
		else if (_alloc() == other._alloc()) {
			_begin()       = other._begin();
			_end()         = other._end()  ;
			_storage_end() = other._storage_end();
			other._drop_ownership();
		}
		else {
			reserve(other.size());
			for (auto& x : other)
				push_back(std::move(x));
			// do not clear other; we don't need to for an unspecified state,
			// and it will be destroyed separately
		}
		return *this;
	}

	CONSTEXPR void clear() noexcept {
		erase(begin(), end());}

private:
	CONSTEXPR void _destroy() noexcept {
		clear();
		// Can't have clear reduce the capacity, so the storage is deallocated manually here
		_alloc_traits::deallocate(_alloc(), _begin(), capacity());
	}

public:
	CONSTEXPR ~vector() /* noexcept is implicit :) */ {
		_destroy();}

	CONSTEXPR size_type size() const noexcept {return _end() - _begin();}
	CONSTEXPR size_type capacity() const noexcept {return _storage_end() - _begin();}
	CONSTEXPR void shrink_to_fit() noexcept {} // Ignore this for now (no requirement to implement it anyway)

	// Use ref-qualifiers
	CONSTEXPR reference operator[](size_type index) & noexcept {
		return begin()[index];}
	CONSTEXPR const_reference operator[](size_type index) const& noexcept {
		return begin()[index];}
	CONSTEXPR value_type&& operator[](size_type index) && noexcept {
		return std::move(begin()[index]);}

	// Use ref-qualifiers
	CONSTEXPR reference at(size_type index) & {
		if (index >= size()) throw std::out_of_range{"vector::at: out of bounds index"};
		return begin()[index];
	}
	CONSTEXPR const_reference at(size_type index) const& {
		if (index >= size()) throw std::out_of_range{"vector::at: out of bounds index"};
		return begin()[index];
	}
	CONSTEXPR value_type&& at(size_type index) && {
		if (index >= size()) throw std::out_of_range{"vector::at: out of bounds index"};
		return std::move(begin()[index]);
	}

	// Use two separate overloads, otherwise you superfluously require copy constructability of the value type
	CONSTEXPR void resize(size_type s) {
		if (s < size())
			erase(begin() + s, end());
		else while (s > size())
			emplace_back();
	}
	CONSTEXPR void resize(size_type s, const_reference val) {
		if (s < size())
			erase(begin() + s, end());
		else while (s > size())
			emplace_back(val);
	}

private:

	CONSTEXPR pointer cit_to_ptr(const_iterator it) noexcept {
		return const_cast<pointer>(const_pointer{it});
	}

	// strongly exception-safe construction of a sequence of elements.
	template <typename F>
	CONSTEXPR void _safe_construct_sequence(pointer it, size_type count, F const& f) {
		auto end = it;
		for (size_type i = 0; i < count; ++i) try {
			_alloc_traits::construct(_alloc(), end, f(i));
			++end;
		} catch (...) {
			while (it != end)
				_alloc_traits::destroy(_alloc(), it++);
			throw;
		}
	}

	template <typename F>
	CONSTEXPR void _try_after_reallocate(size_type newcap, F const& trial_construction) {
		const size_type new_capacity = newcap * 1.5;// To avoid reallocating repeatedly
		// hint at right after the current allocation
		auto begin_new = _alloc_traits::allocate(_alloc(), new_capacity, _storage_end());
		try {
			_safe_construct_sequence(begin_new, size(),
			                         [iter = begin()] (auto i) -> decltype(auto) {return std::move(iter[i]);});
		} catch(...) {
			_alloc_traits::deallocate(_alloc(), begin_new, new_capacity);
			throw; // rethrow
		}
		try {
			trial_construction(begin_new);
		} catch (...) {
			for (size_type i = 0; i < size(); ++i)
				_alloc_traits::destroy(_alloc(), begin_new + i);
			_alloc_traits::deallocate(_alloc(), begin_new, new_capacity);
			throw; // rethrow
		}
		auto old_size = size();
		_destroy();
		_begin() = begin_new;
		_end() = begin_new + old_size;
		_storage_end() = begin_new + new_capacity;
	}

	CONSTEXPR void _reallocate(size_type newcap) {
		_try_after_reallocate(newcap, [](auto){});}

	CONSTEXPR std::tuple<pointer, pointer, pointer> _shift_forward(pointer pos, size_type n) {
		const auto until_end = _end() - pos;
		const auto constructions = std::min<size_type>(n, until_end); // number of constructions performed
		// TODO: Make this more efficient. Can save a series of move assignments here
		reserve(size() + n);
		pos = _end() - until_end;
		if (until_end > 0) {
			// First, move the elements that go into unconstructed slots.
			_safe_construct_sequence(_end() + (n-constructions), constructions,
			                         [pos] (auto i) -> decltype(auto) {return std::move(pos[i]);});

			_end() += n;

			/* Cover the remaining elements (if any). This is basic exception safe, because
			   if any move fails, nothing is leaked and the vector is safely destructible. */
			if (n < until_end)
				std::move_backward(pos, _end() - n, _end());
			return {pos, pos + constructions, pos + n};
		}
		_end() += n;
		return {pos, pos, _end()};
	}

public:

	CONSTEXPR void reserve(size_type cap) {
		if (cap > capacity())
			_reallocate(cap);
	}

	CONSTEXPR void insert(const_iterator pos, size_type count, const_reference val) {
		auto p = cit_to_ptr(pos);
		pointer end_constructed, start_shifted;
		std::tie(p, end_constructed, start_shifted) = _shift_forward(p, count);
		std::fill(p, end_constructed, val);
		_safe_construct_sequence(end_constructed, start_shifted-end_constructed,
		                         [&val] (auto) -> decltype(auto) {return val;});
	}

	template <typename InputIterator, typename Sentinel>
	CONSTEXPR std::enable_if_t<detail::hasCategory<InputIterator, std::input_iterator_tag>>
	  insert(const_iterator pos, InputIterator first, Sentinel last) {
		if constexpr(isMultiPass<InputIterator>) {
			auto p = cit_to_ptr(pos);
			pointer end_constructed, start_shifted;
			std::tie(p, end_constructed, start_shifted) = _shift_forward(p, detail::distance(first, last));
			std::copy_n(first, end_constructed-p, p);
			first += end_constructed-p;
			try {
				_safe_construct_sequence(end_constructed, start_shifted-end_constructed,
				                         [first] (auto i) {return first[i];});
			} catch(...) {
				// on an exception, destroy all constructed elements in the new block, then adjust 'end'
				while (start_shifted != _end())
					_alloc_traits::destroy(_alloc(), start_shifted++);
				_end() = end_constructed;
				throw;
			}
		}
		else while (first != last)
			pos = insert(pos, *first++) + 1;
	}

	CONSTEXPR void insert(const_iterator pos, std::initializer_list<value_type> ilist) {
		insert(pos, ilist.begin(), ilist.end());
	}

	template <typename... Args>
	CONSTEXPR iterator emplace(const_iterator it, Args&&... args) {
		auto p = cit_to_ptr(it);
		if (it == end())  { // if we insert at the end, construct
			if (size() < capacity()) 
				_alloc_traits::construct(_alloc(), _end(), std::forward<Args>(args)...);
			else {
				auto s = size();
				// GCC ICEs here if the explicit capture is removed.
				// A similar bug was mentioned in a Stackoverflow Question I answered:
				// http://stackoverflow.com/a/40106926/3647361
				// Apparently GCC has trouble with reference-capturing this?
				_try_after_reallocate(s+1, [&, this] (auto new_begin) {
					_alloc_traits::construct(_alloc(), new_begin+s, std::forward<Args>(args)...);});
			}
			return _end()++;
		}
		else {
			std::aligned_storage_t<sizeof(value_type), alignof(value_type)> storage;
			auto& obj = reinterpret_cast<reference>(storage);
			_alloc_traits::construct(_alloc(), &obj, std::forward<Args>(args)...);
			detail::OnDestruction _([&]{_alloc_traits::destroy(_alloc(), &obj);});

			pointer s_cons, e_cons, e;
			std::tie(s_cons, e_cons, e) = _shift_forward(p, 1);
			*s_cons = std::move(obj);
            return iterator{s_cons};
		}
	}
	CONSTEXPR iterator insert(const_iterator it, const_reference val) {
		return emplace(it, val);}
	CONSTEXPR iterator insert(const_iterator it, value_type&& val) {
		return emplace(it, std::move(val));}

	CONSTEXPR iterator erase(const_iterator first, const_iterator last) {
		auto length = last-first;
		for (auto p = cit_to_ptr(first); last != end();)
			*p++ = std::move(*last++);
		_end() -= length;
		for (size_type i = 0; i < length; ++i)
			_alloc_traits::destroy(_alloc(), _end() + i);

		return iterator{cit_to_ptr(first)};
	}

	CONSTEXPR iterator erase(const_iterator pos) {
		return erase(pos, pos+1); }

	CONSTEXPR void pop_back() noexcept {
		erase(end()-1, end());}

	template <typename InputIterator, typename Sentinel>
	CONSTEXPR std::enable_if_t<detail::hasCategory<InputIterator, std::input_iterator_tag>>
	  assign(InputIterator first, Sentinel last) {
		for (auto it = begin(); it != end() && first != last;)
			*it++ = *first++;
		insert(end(), first, last);
	}
	CONSTEXPR void assign(std::initializer_list<value_type> ilist) {
		assign(ilist.begin(), ilist.end());}
	CONSTEXPR void assign(size_type count, const_reference val) {
		for (auto it = begin(); it != end() && count-- > 0;)
			*it++ = val;
		insert(end(), count, val);
	}

	template <typename... Args>
	CONSTEXPR void emplace_back(Args&&... args) {
		emplace(end(), std::forward<Args>(args)...);}

	CONSTEXPR void push_back(T const& element) {emplace_back(element);}
	CONSTEXPR void push_back(T&& element) {emplace_back(std::move(element));}

	CONSTEXPR void swap(vector& other) noexcept {
		using std::swap; // see [swappable.requirements]/3. Pointer type can be user-defined!
		swap(_begin()      , other._begin());
		swap(_end()        , other._end());
		swap(_storage_end(), other._storage_end());
		// see [container.requirements.general]/9.
		if constexpr(_alloc_traits::propagate_on_container_swap::value) {
			swap(_alloc(), other._alloc());
		}
	}

	CONSTEXPR pointer data() noexcept {return _begin();}
	CONSTEXPR const_pointer data() const noexcept {return _begin();}
};

#include <iostream>

struct Test {
	int id = 0;
	static int counter;
	Test(int i) : id(i) {++counter;}
	Test(Test&& o) : id(o.id) {if (counter > 150) throw 0; ++counter; }
	Test& operator=(Test&& t) {if (counter > 100) throw 0; id = t.id; return *this;}
	Test& operator=(Test const& t) = default;
	Test(Test const& o) : id(o.id) {counter++;}
	~Test() {--counter;}
	Test() {++counter;}
};
int Test::counter;

int main() {
	try {
		vector<Test> array{0, 1, 2, 3};
		array.insert(array.begin(), 684); // {684, 0, 1, 2, 3}
		array.erase(array.begin()+3, array.end()); // {684, 0, 1}
		array.insert(array.begin()+2, {1, 2, 3, 4, 5, 6}); // {684, 0, 1, 2, 3, 4, 5, 6, 1}
		for (auto x : array) std::cout << x.id << ", ";
	} catch(...) {}
	std::cout << '\n' << Test::counter;
}
