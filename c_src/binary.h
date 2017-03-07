#include <iostream>
#include <algorithm>
#include <string.h>

class Binary {
    public:
    unsigned char *bin;
    size_t size;
    bool allocated;

    Binary() : bin(NULL), size(0), allocated(false) { }
    Binary(const char *data) {
	bin = (unsigned char *) data;
	size = strlen(data);
	allocated = false;
    }
    
    Binary(const Binary &b) {
	bin = b.bin;
	size = b.size;
	allocated = false;
    }

    ~Binary() {
	if (allocated) {
	    delete bin;
	}
    }

    operator std::string() {
	return (const char *) bin;
    }

    friend std::ostream & operator<<(std::ostream & str, Binary const &b) {
	return str << b.bin;
    }

    bool operator<(const Binary &b) {
	if(size < b.size) {
	  return true;
	} else if (size > b.size) {
	  return false;
	} else {
	  return memcmp(bin,b.bin,size) < 0;
	}
    }

    bool operator<(Binary &b) {
	if(size < b.size) {
	  return true;
	} else if (size > b.size) {
	  return false;
	} else {
	  return memcmp(bin,b.bin,size) < 0;
	}
    }

    bool operator>(const Binary &b) {
	if(size > b.size) {
	  return true;
	} else if (size < b.size) {
	  return false;
	} else {
	  return memcmp(bin,b.bin,size) > 0;
	}
    }

    bool operator== (const Binary &b) {
	if (size == b.size ) {
	    return memcmp(bin,b.bin, std::min(size, b.size)) == 0;
	} else {
	  return false;
	}
    }
    operator std::string() const {
	return (const char*) bin;
    }

    Binary& set_data(const char *data) {
	bin = (unsigned char *) data;
	size = strlen(data);
	return *this;
    }

    void copy(char *inbin, size_t insize) {
	bin = (unsigned char *) operator new(insize);
	allocated = true;
	size = insize;
	memcpy(bin, inbin, size);
    }
};

inline bool operator < (const Binary &a, const Binary &b) {

	if(a.size < b.size) {
	  return true;
	} else if (a.size > b.size) {
	  return false;
	} else {
	  return memcmp(a.bin,b.bin, std::min(a.size, b.size)) < 0;
	}
}

