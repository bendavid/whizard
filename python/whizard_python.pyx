from cpython cimport array
cimport cwhizard

import array

cdef class WhizardSample:
    """Whizard Sample Python Class
    """

    cdef cwhizard.sample_handle_t _c_sample

    def __cinit__(self, Whizard wz, str name):
        cdef array.array c_name = array.array('b', name.encode('UTF-8') + b'\0')
        cwhizard.whizard_new_sample(
            &wz._c_whizard,
            c_name.data.as_chars,
            &self._c_sample)
        if self._c_sample is NULL:
            raise MemoryError()

    def __dealloc__(self):
        if self._c_sample is not NULL:
            cwhizard.whizard_sample_close(&self._c_sample)

    cpdef (int, int) open(self):
        cdef int it_begin, it_end
        cwhizard.whizard_sample_open(&self._c_sample, &it_begin, &it_end)
        return it_begin, it_end

    cpdef next_event(self):
        cwhizard.whizard_sample_next_event(&self._c_sample)

    cpdef close(self):
        del(self)

    cpdef int get_event_index(self):
        cdef int idx
        cwhizard.whizard_sample_get_event_index(&self._c_sample, &idx)
        return idx

    cpdef int get_process_index(self):
        cdef int i_proc
        cwhizard.whizard_sample_get_process_index(&self._c_sample, &i_proc)
        return i_proc

    cpdef str get_process_id(self):
        cdef int strlen = cwhizard.whizard_sample_get_process_id_len(&self._c_sample) + 1
        cdef array.array c_proc_id = array.array('b', [])
        array.resize(c_proc_id, strlen) # Caveat: inconsistency in the strlen handling!
        cwhizard.whizard_sample_get_process_id(
            &self._c_sample,
            c_proc_id.data.as_chars,
            strlen)
        return c_proc_id.tobytes().decode('UTF-8')[:-1]

    cpdef double get_fac_scale(self):
        cdef double f_scale
        cwhizard.whizard_sample_get_fac_scale(&self._c_sample, &f_scale)
        return f_scale

    cpdef double get_alpha_s(self):
        cdef double alpha_s
        cwhizard.whizard_sample_get_alpha_s(&self._c_sample, &alpha_s)
        return alpha_s

    cpdef double get_weight(self):
        cdef double weight
        cwhizard.whizard_sample_get_weight(&self._c_sample, &weight)
        return weight

    cpdef double get_sqme(self):
        cdef double sqme
        cwhizard.whizard_sample_get_sqme(&self._c_sample, &sqme)
        return sqme


cdef class Whizard:
    """Whizard Python Class

    >>>> wz = whizard.Whizard()
    >>>> wz.option(...) # set commandline options before initialization
    >>>> wz.init()
    >>>> wz.set_double("<var>", 500.)
    """
    cdef cwhizard.whizard_t _c_whizard

    def __cinit__(self):
        # Only access cdef fields in self
        cwhizard.whizard_create(&self._c_whizard)
        if self._c_whizard is NULL:
            raise MemoryError()

    def __dealloc__(self):
        if self._c_whizard is not NULL:
            cwhizard.whizard_final(&self._c_whizard)

    cpdef option(self, str key, str value):
        # https://docs.cython.org/en/latest/src/tutorial/strings.html#encoding-text-to-bytes
        # https://docs.python.org/3/library/array.html
        # https://cython.readthedocs.io/en/latest/src/tutorial/array.html
        # The string handling function string_c2f expects '\0'-terminated character string.
        # Thus, we need to append a null character (as binary).
        cdef array.array c_key = array.array('b', key.encode('UTF-8') + b'\0')
        cdef array.array c_value = array.array('b', value.encode('UTF-8') + b'\0')
        cwhizard.whizard_option(
            &self._c_whizard,
            c_key.data.as_chars,
            c_value.data.as_chars)

    cpdef init(self):
        cwhizard.whizard_init(&self._c_whizard)

    cpdef set_double(self, str var, double value):
        cdef array.array c_var = array.array('b', var.encode('UTF-8') + b'\0')
        cwhizard.whizard_set_double(
            &self._c_whizard,
            c_var.data.as_chars,
            value)

    cpdef set_int(self, str var, int value):
        cdef array.array c_var = array.array('b', var.encode('UTF-8') + b'\0')
        cwhizard.whizard_set_int(
            &self._c_whizard,
            c_var.data.as_chars,
            value)

    cpdef set_bool(self, str var, bint value):
        cdef array.array c_var = array.array('b', var.encode('UTF-8') + b'\0')
        cwhizard.whizard_set_bool(
            &self._c_whizard,
            c_var.data.as_chars,
            value)

    cpdef set_string(self, str var, str value):
        cdef array.array c_var = array.array('b', var.encode('UTF-8') + b'\0')
        cdef array.array c_value = array.array('b', value.encode('UTF-8') + b'\0')
        cwhizard.whizard_set_char(
            &self._c_whizard,
            c_var.data.as_chars,
            c_value.data.as_chars)

    cpdef double get_double(self, str var) except? 0:
        cdef array.array c_var = array.array('b', var.encode('UTF-8') + b'\0')
        cdef double value
        cdef bint err = cwhizard.whizard_get_double(
            &self._c_whizard,
            c_var.data.as_chars,
            &value)
        if err:
            raise IndexError("Cannot retrieve double variable from WHIZARD.")
        return value

    cpdef int get_int(self, str var) except? 0:
        cdef array.array c_var = array.array('b', var.encode('UTF-8') + b'\0')
        cdef int value
        cdef bint err = cwhizard.whizard_get_int(
            &self._c_whizard,
            c_var.data.as_chars,
            &value)
        if err:
            raise IndexError("Cannot retrieve int variable from WHIZARD.")
        return value

    cpdef bint get_bool(self, str var) except? False:
        cdef array.array c_var = array.array('b', var.encode('UTF-8') + b'\0')
        cdef int value = 0
        cdef bint err = cwhizard.whizard_get_bool(
            &self._c_whizard,
            c_var.data.as_chars,
            &value)
        if err:
            raise IndexError("Cannot retrieve boolean variable from WHIZARD.")
        return value

    cpdef str get_string(self, str var):
        cdef array.array c_var = array.array('b', var.encode('UTF-8') + b'\0')
        cdef int strlen = cwhizard.whizard_get_char_len(
            &self._c_whizard,
            c_var.data.as_chars)
        cdef array.array c_value = array.array('b', [])
        array.resize(c_value, strlen)
        cdef bint err = cwhizard.whizard_get_char(
            &self._c_whizard,
            c_var.data.as_chars,
            c_value.data.as_chars,
            strlen)
        if err:
            raise IndexError("Cannot retrieve string variable from WHIZARD.")
        # Remove trailing null character from C.
        return c_value.tobytes().decode('UTF-8')[:-1]

    cpdef str flv_string(self, int pdg):
        cdef int strlen = cwhizard.whizard_flv_string_len(&self._c_whizard, pdg)
        cdef array.array c_str = array.array('b', [])
        array.resize(c_str, strlen)
        cdef bint err = cwhizard.whizard_flv_string(
            &self._c_whizard,
            pdg,
            c_str.data.as_chars,
            strlen)
        if err:
            raise IndexError("Cannot retrieve string variable from WHIZARD.")
        # Remove trailing null character from C.
        return c_str.tobytes().decode('UTF-8')[:-1]

    cpdef str flv_array_string(self, list flavors):
        cdef array.array fa = array.array('i', flavors)
        cdef array.array c_str = array.array('b', [])
        cdef int strlen = cwhizard.whizard_flv_array_string_len(
            &self._c_whizard,
            fa.data.as_ints,
            len(fa))
        array.resize(c_str, strlen)
        cdef bint err = cwhizard.whizard_flv_array_string(
            &self._c_whizard,
            &fa.data.as_ints[0],
            len(fa),
            c_str.data.as_chars, strlen)
        # Remove trailing null character from C.
        return c_str.tobytes().decode('UTF-8')[:-1]

    cpdef command(self, str cmd):
        cdef array.array c_cmd = array.array('b', cmd.encode('UTF-8') + b'\0')
        cwhizard.whizard_command(
            &self._c_whizard,
            c_cmd.data.as_chars)

    cpdef (double, double) get_integration_result(self, str proc_id):
        cdef array.array c_proc_id = array.array('b', proc_id.encode('UTF-8') + b'\0')
        cdef double integral, error
        cdef bint err = cwhizard.whizard_get_integration_result(
            &self._c_whizard,
            c_proc_id.data.as_chars,
            &integral,
            &error)
        return integral, error

    cpdef WhizardSample new_sample(self, name):
        return WhizardSample(self, name)

