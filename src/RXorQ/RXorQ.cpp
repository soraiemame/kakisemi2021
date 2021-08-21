// ABC185F Range Xor Query https://atcoder.jp/contests/abc185/tasks/abc185_f
// Time 57 64 61

#include <bits/stdc++.h>
#include <atcoder/all>
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


int op(int a,int b){ return a ^ b; }
int e(){ return 0; }

int main() {
    int n,q;
    cin >> n >> q;
    vector<int> a(n);
  	for(int i = 0;i < n;i++)cin >> a[i];
    atcoder::segtree<int,op,e> st(a);
  	while(q--){
      	int t,x,y;
        cin >> t >> x >> y;
      	if(t == 1){
          	st.set(x - 1,st.get(x - 1) ^ y);
        }
        else{
        	cout << st.prod(x - 1,y) << "\n";
        }
    }
}
