// Range Add Query https://onlinejudge.u-aizu.ac.jp/courses/library/3/DSL/2/DSL_2_B
// Time 00.00 00.00 00.00

#include <bits/stdc++.h>

namespace atcoder {

namespace internal {

// @param n `0 <= n`
// @return minimum non-negative `x` s.t. `n <= 2**x`
int ceil_pow2(int n) {
    int x = 0;
    while ((1U << x) < (unsigned int)(n)) x++;
    return x;
}

// @param n `1 <= n`
// @return minimum non-negative `x` s.t. `(n & (1 << x)) != 0`
int bsf(unsigned int n) {
#ifdef _MSC_VER
    unsigned long index;
    _BitScanForward(&index, n);
    return index;
#else
    return __builtin_ctz(n);
#endif
}

}  // namespace internal

}  // namespace atcoder


namespace atcoder {

template <class S, S (*op)(S, S), S (*e)()> struct segtree {
  public:
    segtree() : segtree(0) {}
    explicit segtree(int n) : segtree(std::vector<S>(n, e())) {}
    explicit segtree(const std::vector<S>& v) : _n(int(v.size())) {
        log = internal::ceil_pow2(_n);
        size = 1 << log;
        d = std::vector<S>(2 * size, e());
        for (int i = 0; i < _n; i++) d[size + i] = v[i];
        for (int i = size - 1; i >= 1; i--) {
            update(i);
        }
    }

    void set(int p, S x) {
        assert(0 <= p && p < _n);
        p += size;
        d[p] = x;
        for (int i = 1; i <= log; i++) update(p >> i);
    }

    S get(int p) const {
        assert(0 <= p && p < _n);
        return d[p + size];
    }

    S prod(int l, int r) const {
        assert(0 <= l && l <= r && r <= _n);
        S sml = e(), smr = e();
        l += size;
        r += size;

        while (l < r) {
            if (l & 1) sml = op(sml, d[l++]);
            if (r & 1) smr = op(d[--r], smr);
            l >>= 1;
            r >>= 1;
        }
        return op(sml, smr);
    }

    S all_prod() const { return d[1]; }

    template <bool (*f)(S)> int max_right(int l) const {
        return max_right(l, [](S x) { return f(x); });
    }
    template <class F> int max_right(int l, F f) const {
        assert(0 <= l && l <= _n);
        assert(f(e()));
        if (l == _n) return _n;
        l += size;
        S sm = e();
        do {
            while (l % 2 == 0) l >>= 1;
            if (!f(op(sm, d[l]))) {
                while (l < size) {
                    l = (2 * l);
                    if (f(op(sm, d[l]))) {
                        sm = op(sm, d[l]);
                        l++;
                    }
                }
                return l - size;
            }
            sm = op(sm, d[l]);
            l++;
        } while ((l & -l) != l);
        return _n;
    }

    template <bool (*f)(S)> int min_left(int r) const {
        return min_left(r, [](S x) { return f(x); });
    }
    template <class F> int min_left(int r, F f) const {
        assert(0 <= r && r <= _n);
        assert(f(e()));
        if (r == 0) return 0;
        r += size;
        S sm = e();
        do {
            r--;
            while (r > 1 && (r % 2)) r >>= 1;
            if (!f(op(d[r], sm))) {
                while (r < size) {
                    r = (2 * r + 1);
                    if (f(op(d[r], sm))) {
                        sm = op(d[r], sm);
                        r--;
                    }
                }
                return r + 1 - size;
            }
            sm = op(d[r], sm);
        } while ((r & -r) != r);
        return 0;
    }

  private:
    int _n, size, log;
    std::vector<S> d;

    void update(int k) { d[k] = op(d[2 * k], d[2 * k + 1]); }
};

}  // namespace atcoder


using namespace std;

namespace fastio{
    static constexpr size_t buf_size = 1 << 18;
    static constexpr size_t integer_size = 20;
    static constexpr size_t block_size = 10000;

    static char inbuf[buf_size + 1] = {};
    static char outbuf[buf_size + 1] = {};
    static char block_str[block_size * 4 + 1] = {};

    static constexpr uint64_t power10[] = {
        1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000,
        1000000000, 10000000000, 100000000000, 1000000000000, 10000000000000,
        100000000000000, 1000000000000000, 10000000000000000, 100000000000000000,
        1000000000000000000, 10000000000000000000u
    };

    struct Scanner {
        private:
        size_t pos,end;

        void load() {
            end = fread(inbuf,1,buf_size,stdin);
            inbuf[end] = '\0';
        }
        void reload() {
            size_t len = end - pos;
            memmove(inbuf,inbuf + pos,len);
            end = len + fread(inbuf + len,1,buf_size - len,stdin);
            inbuf[end] = '\0';
            pos = 0;
        }
        void skip_space() {
            while(inbuf[pos] <= ' '){
                if(__builtin_expect(++pos == end, 0)) reload();
            }
        }
        char get_next() { return inbuf[pos++]; }
        char get_next_nonspace() {
            skip_space();
            return inbuf[pos++];
        }
        public:
        Scanner() { load(); }

        void scan(char& c) { c = get_next_nonspace(); }
        void scan(std::string& s){
            skip_space();
            s = "";
            do {
                size_t start = pos;
                while (inbuf[pos] > ' ') pos++;
                s += std::string(inbuf + start, inbuf + pos);
                if (inbuf[pos] !='\0') break;
                reload();
            } while (true);
        }

        template <class T>
        typename std::enable_if<std::is_integral<T>::value, void>::type scan(T &x) {
            char c = get_next_nonspace();
            if(__builtin_expect(pos + integer_size >= end, 0)) reload();
            bool neg = false;
            if (c == '-') neg = true, x = 0;
            else x = c & 15;
            while ((c = get_next()) >= '0') x = x * 10 + (c & 15);
            if (neg) x = -x;
        }

        template <class Head, class... Others>
        void scan(Head& head, Others&... others) {
            scan(head); scan(others...);
        }

        template <class T>
        Scanner& operator >> (T& x) {
            scan(x); return *this;
        }
    };

    struct Printer {
        private:
        size_t pos = 0;
        
        void flush() {
            fwrite(outbuf, 1, pos, stdout);
            pos = 0;
        }

        void pre_calc() {
            for (size_t i = 0; i < block_size; i++) {
                size_t j = 4, k = i;
                while (j--) {
                    block_str[i * 4 + j] = k % 10 + '0';
                    k /= 10;
                }
            }
        }

        static constexpr size_t get_integer_size(uint64_t n) {
            if(n >= power10[10]) {
                if (n >= power10[19]) return 20;
                if (n >= power10[18]) return 19;
                if (n >= power10[17]) return 18;
                if (n >= power10[16]) return 17;
                if (n >= power10[15]) return 16;
                if (n >= power10[14]) return 15;
                if (n >= power10[13]) return 14;
                if (n >= power10[12]) return 13;
                if (n >= power10[11]) return 12;
                return 11;
            }
            else {
                if (n >= power10[9]) return 10;
                if (n >= power10[8]) return 9;
                if (n >= power10[7]) return 8;
                if (n >= power10[6]) return 7;
                if (n >= power10[5]) return 6;
                if (n >= power10[4]) return 5;
                if (n >= power10[3]) return 4;
                if (n >= power10[2]) return 3;
                if (n >= power10[1]) return 2;
                return 1;
            }
        }

        public:
        Printer() { pre_calc(); }
        ~Printer() { flush(); }

        void print(char c){
            outbuf[pos++] = c;
            if (__builtin_expect(pos == buf_size, 0)) flush();
        }
        void print(const char *s) {
            while(*s != 0) {
                outbuf[pos++] = *s++;
                // if (pos == buf_size) flush();
                if (__builtin_expect(pos == buf_size, 0)) flush();
            }
        }
        void print(const std::string& s) {
            for(auto c : s){
                outbuf[pos++] = c;
                // if (pos == buf_size) flush();
                if (__builtin_expect(pos == buf_size, 0)) flush();
            }
        }

        template <class T>
        typename std::enable_if<std::is_integral<T>::value, void>::type print(T x) {
            if (__builtin_expect(pos + integer_size >= buf_size, 0)) flush();
            if (x < 0) print('-'), x = -x;
            size_t digit = get_integer_size(x);
            size_t len = digit;
            while (len >= 4) {
                len -= 4;
                memcpy(outbuf + pos + len, block_str + (x % block_size) * 4, 4);
                x /= block_size;
            }
            memcpy(outbuf + pos, block_str + x * 4 + (4 - len), len);
            pos += digit;
        }

        template <class Head, class... Others>
        void print(const Head& head, const Others&... others){
            print(head); print(' '); print(others...);
        }

        template <class... Args>
        void println(const Args&... args) {
            print(args...); print('\n');
        }
        
        template <class T>
        Printer& operator << (const T& x) {
            print(x); return *this;
        }
    };
};

#ifndef _SORAIE
fastio::Scanner fin;
fastio::Printer fout;
#define cin fin
#define cout fout
#endif


int op(int a,int b){ return a + b; }
int e(){ return 0; }

int main() {
    int n,q;
    cin >> n >> q;
    atcoder::segtree<int,op,e> st(n);
  	while(q--){
      	int t,x,y;
        cin >> t >> x >> y;
      	if(t == 0){
          	st.set(x - 1,st.get(x - 1) + y);
        }
        else{
        	cout << st.prod(x - 1,y) << "\n";
        }
    }
}
