#include <iostream>
#include <ios>
#include <fstream>
#include <streambuf>
#include <vector>
#include <exception>
#include <stdexcept>
#include <string>
#include <functional>
#include <future>
#include <algorithm>
#include <utility>
#include <chrono>

template <typename T> using ifstream = std::basic_ifstream<T, std::char_traits<T>>;
template <typename T> using istream = std::basic_istream<T, std::char_traits<T>>;
template <typename T> using ostream = std::basic_ostream<T, std::char_traits<T>>;
template <typename T> using string = std::basic_string<T, std::char_traits<T>, std::allocator<T>>;
template <typename T> using int_type = typename std::template char_traits<T>::int_type;

template<typename T> class match
{
public:
    match(size_t _line, size_t _col, string<T>&& _str)
        :m_line(_line)
        ,m_col(_col)
        ,m_str(_str)
    {}
    size_t getline()
    {
        return m_line;
    }
    size_t setline(size_t _line)
    {
        return m_line = _line;
    }
    size_t getcol()
    {
        return m_col;
    }
    size_t setcol(size_t _col)
    {
        return m_col = _col;
    }
    string<T>& getstr()
    {
        return m_str;
    }
    void print(ostream<T>& _os)
    {
        _os << m_line+1 << _os.widen(' ') << m_col+1 << _os.widen(' ') << m_str << std::endl;
    }
private:
    size_t m_line;
    size_t m_col;
    string<T> m_str;
};
template<typename T> ostream<T>& operator<<(ostream<T>& _os, match<T>& _match)
{
    _match.print(_os);
    return _os;
}
template<typename T> class comparer;
template<typename T> class mask
{
public:
    mask() = delete;
    mask(const string<T>& _mask, const T _anychar)
        :m_mask(_mask)
        , m_anychar(_anychar)
        , m_lps(genlps())
    {}
    auto size() const
    {
        return m_mask.size();
    }
private:
    friend comparer<T>;
    std::vector<int> genlps()
    {
        std::vector<int> _lps(m_mask.size());

        int len;
        int i;
        int ign;
        for (ign = 0; ign < m_mask.size(); ign++)
        {
            if (m_anychar != m_mask[ign])
            {
                _lps[ign] = ign;
                break;
            }
            else
                _lps[ign] = ign+1;
        }
        len = ign;
        i = ign + 1;
        while (i < m_mask.size())
        {
            if (m_mask[i] == m_mask[len]
                || m_anychar == m_mask[len])
            {
                len++;
                _lps[i] = len;
                i++;
            }
            else
            {
                if (len > ign)
                {
                    len = _lps[len - 1];
                }
                else
                {
                    _lps[i] = ign;//0
                    i++;
                }
            }
        }
        return _lps;
    }
    const string<T> m_mask;
    const int_type<T> m_anychar;
    std::vector<int> m_lps;//For KMP alg
};
template<typename T> class comparer
{
public:
    comparer(const mask<T>& _mask)
        :m_mask(_mask)
        ,m_str(m_mask.size(), 0)
    {}
    int comparenext(T _ch)
    {
        m_str[strpoiner] = _ch;
        strpoiner++;
        if (strpoiner >= m_str.size()) strpoiner = 0;

        if (std::char_traits<T>::eq(m_mask.m_mask[cur], m_mask.m_anychar)
            || std::char_traits<T>::eq(m_mask.m_mask[cur], _ch))
            cur++;
        else if(cur)
            cur = m_mask.m_lps[cur-1];

        if (cur >= m_mask.size())
        {
            cur = 0;
            return 1;
        }
        return 0;
    }
    string<T> getstr()
    {
        string<T> str;
        str.reserve(m_str.size());
        str = m_str.substr(strpoiner, m_str.size());
        str += m_str.substr(0, strpoiner);
        return str;
    }
    void reset()
    {
        cur = 0;
        strpoiner = 0;
    }
private:
    const mask<T>& m_mask;
    string<T> m_str;
    size_t cur = 0;
    size_t strpoiner = 0;
};
template<typename T> class matches
{
public:
    matches(){}
    void setLines(size_t _lines)
    {
        m_lines = _lines;
    }
    void setlastlinelenght(size_t _len)
    {
        m_lastlinelenght = _len;
    }
    void print(ostream<T>& _os)
    {
        _os << m_matches.size() << std::endl;
        for (auto& el : m_matches)
            _os << el;
    }
    void add(size_t _line, size_t _col, string<T>&& _str)
    {
        m_matches.emplace_back(_line, _col, std::move(_str));
    }
    matches<T>& operator<<(matches<T> _other)
    {
        if (_other.m_matches.size())
        {
            //Если в сегменте нет перевода строки то добавляем длинну строки
            if (!_other.m_matches[0].getline())
                _other.m_matches[0].setcol(_other.m_matches[0].getcol() + m_lastlinelenght);

            m_matches.reserve(m_matches.size() + _other.m_matches.size());

            for (auto el : _other.m_matches)
                m_matches.emplace_back(el.getline() + m_lines, el.getcol(), string<T>(el.getstr()));
        }
        m_lines += _other.m_lines;
        m_lastlinelenght = _other.m_lastlinelenght;
        return *this;
    }
private:
    size_t m_lines = 0;
    size_t m_lastlinelenght = 0;
    std::vector<match<T>> m_matches;
};
template<typename T> ostream<T>& operator<<(ostream<T>& _os, matches<T>& _matches)
{
    _matches.print(_os);
    return _os;
}
template<typename T> matches<T> job(ifstream<T> _stream, const size_t _ignore, const size_t _maxchars, const mask<T>& _mask)
{
    matches<T> res;
    comparer<T> comp(_mask);
    size_t line = 0;
    size_t col = 0;
    size_t ignore = _ignore;//Сколько символов в начале нужно проигнорировать чтобы не получить наложение результатов
    for (size_t i = _maxchars; i > 0; i--)
    {
        auto ch = _stream.get();
        if (!std::char_traits<T>::not_eof(ch))
            break;
        if (std::char_traits<T>::eq(ch, _stream.widen('\n')))
        {
            if (i < _mask.size()) break;
            line++;
            col = 0;
            comp.reset();
            if (i <= _mask.size()) break;
        }
        else
        {
            col++;
            if (ignore) ignore--;
            else if (comp.comparenext(std::char_traits<T>::to_char_type(ch)))
                res.add(line, col - _mask.size(), std::move(comp.getstr()));
        }
    }
    res.setLines(line);
    col >= _mask.size() ? col -= _mask.size() - 1 : col = 0;
    res.setlastlinelenght(col);
    return res;
}
template<typename T> matches<T> mtjob(const std::string& _filename, const size_t _maxchars, const mask<T>& _mask, const size_t _threads)
{
    matches<T> res;
    comparer<T> comp(_mask);
    ifstream<T> stream(_filename, std::ios_base::in | std::ios_base::binary);
  
    stream.seekg(0, std::ios_base::end);
    size_t size = stream.tellg();
    if (size == istream<T>::pos_type(-1)) throw std::exception("bad file");
    if (size < _mask.size()) return res;

	size_t threads;
	if (_threads) threads = _threads;
	else threads = std::thread::hardware_concurrency();
	if (!threads) threads = 1;
	size_t tmp = size - _mask.size() + 1;
	threads = threads > tmp ? tmp : threads;

	std::vector<std::future<matches<T>>> vf;
    vf.reserve(threads);

	auto piecesize = size - _mask.size() + 1;
	size_t beg = 0, end, rem = piecesize % threads;
    piecesize /= threads;
    end = piecesize;

	for (size_t i = 0; i < threads; i++)
	{
        if (rem)
        {
            end += 1;
            rem--;
        }
		ifstream<T> f(_filename, std::ios_base::in | std::ios_base::binary);
		f.seekg(beg, std::ios_base::beg);

        //Игнорирование наложения результатов сегментов
        size_t ignore = 0;
        if (i)
        {
            stream.seekg(beg - _mask.size() + 1, std::ios_base::beg);
            for (size_t i = 0; i < (_mask.size() - 1) * 2; i++)
            {
                auto ch = stream.get();
                if (!std::char_traits<T>::not_eof(ch))
                    break;
                if (std::char_traits<T>::eq(ch, stream.widen('\n')))
                    break;
                else if (comp.comparenext(std::char_traits<T>::to_char_type(ch)))
                {
                    ignore = i - 1;
                    break;
                }
            }
            comp.reset();
        }

		vf.emplace_back(std::async(job<T>, std::move(f), ignore, end - beg + _mask.size() - 1, std::ref(_mask)));
        beg = end;
        end += piecesize;
	}
    for (auto& el : vf)
        res << el.get();

    return res;
}

int main(int argc, char* argv[])
{
    if(argc < 3)
    {
        std::cout << "Use: ""filename"" ""mask""\n";
        return 0;
    }
    mask<char> msk(argv[2], '?');

    std::chrono::nanoseconds total_time(0);
    std::chrono::time_point<std::chrono::system_clock> time = std::chrono::system_clock::now();
    auto res(mtjob(argv[1], INT_MAX, msk, 0));
    total_time += std::chrono::system_clock::now() - time;

    std::cout << res;

    std::cout << "time: " << std::chrono::duration_cast<std::chrono::milliseconds>(total_time).count() << "ms" << std::endl;
}
