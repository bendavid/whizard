#include <Python.h>

void c_whizard_process_string(void *w_instance, const char *cmds);
void c_whizard_finalize(void *w_instance);

void c_whizard_model(void *w_instance, const char *model);
void c_whizard_process(void *w_instance, const char *process, const char *in, const char *out);
void c_whizard_compile(void *w_instance);
void c_whizard_beams(void *w_instance, const char *specs);
void c_whizard_integrate(void *w_instance, const char *process);
void c_whizard_matrix_element_test(void *w_instance, const char *process);
void c_whizard_simulate(void *w_instance, const char *process);
void c_whizard_sqrts(void *w_instance, const int *val, const char *unit);
void *w_instance;

static PyObject *

pywhizard_process_string(PyObject *self, PyObject *args)
{
  const char *cmds;
  if (!PyArg_ParseTuple(args, "s", &cmds))
    return NULL;
  c_whizard_process_string(&w_instance, cmds);
  return Py_BuildValue("i", 0);
}

pywhizard_finalize(PyObject *self, PyObject *args)
{
  c_whizard_finalize(&w_instance);
  return Py_BuildValue("i", 0);
}

pywhizard_model(PyObject *self, PyObject *args)
{
  const char *model;
  if (!PyArg_ParseTuple(args, "s", &model))
    return NULL;
  c_whizard_model(&w_instance, model);
  return Py_BuildValue("i", 0);
}

pywhizard_process(PyObject *self, PyObject *args)
{
  const char *process;
  const char *in;
  const char *out;
  if (!PyArg_ParseTuple(args, "sss", &process, &in, &out))
    return NULL;
  c_whizard_process(&w_instance, process, in, out);
  return Py_BuildValue("i", 0);
}

pywhizard_compile(PyObject *self, PyObject *args)
{
  c_whizard_compile(&w_instance);
  return Py_BuildValue("i", 0);
}

pywhizard_beams(PyObject *self, PyObject *args)
{
  const char *specs;
  if (!PyArg_ParseTuple(args, "s", &specs))
    return NULL;
  c_whizard_beams(&w_instance, specs);
  return Py_BuildValue("i", 0);
}

pywhizard_integrate(PyObject *self, PyObject *args)
{
  const char *process;
  if (!PyArg_ParseTuple(args, "s", &process))
    return NULL;
  c_whizard_integrate(&w_instance, process);
  return Py_BuildValue("i", 0);
}

pywhizard_matrix_element_test(PyObject *self, PyObject *args)
{
  const char *process;
  if (!PyArg_ParseTuple(args, "s", &process))
    return NULL;
  c_whizard_matrix_element_test(&w_instance, process);
  return Py_BuildValue("i", 0);
}

pywhizard_simulate(PyObject *self, PyObject *args)
{
  const char *process;
  if (!PyArg_ParseTuple(args, "s", &process))
    return NULL;
  c_whizard_simulate(&w_instance, process);
  return Py_BuildValue("i", 0);
}

pywhizard_sqrts(PyObject *self, PyObject *args)
{
  const char *unit;
  const int *val;
  if (!PyArg_ParseTuple(args, "is", &val, &unit))
    return NULL;
  c_whizard_sqrts(&w_instance, &val, unit);
  return Py_BuildValue("i", 0);
}

static PyMethodDef WhizardMethods[] = {
  {"process_string", pywhizard_process_string, METH_VARARGS, "Executes a given Sindarin command."},
  {"finalize", pywhizard_finalize, METH_VARARGS, "Finalize the PyWhizard session."},
  {"model", pywhizard_model, METH_VARARGS, "Changes to given model."},
  {"process", pywhizard_process, METH_VARARGS, "Defines a new process."},
  {"compile", pywhizard_compile, METH_VARARGS, "Compiles the current process to a shared library."},
  {"beams", pywhizard_beams, METH_VARARGS, "Sets up the beams parameters."},
  {"integrate", pywhizard_integrate, METH_VARARGS, "Integrates a process."},
  {"matrix_element_test", pywhizard_matrix_element_test, METH_VARARGS, "Tests the matrix elements of a process."},
  {"simulate", pywhizard_simulate, METH_VARARGS, "Simulates a process."},
  {"sqrts", pywhizard_sqrts, METH_VARARGS, "Sets the com energy for the collisions."},
  {NULL, NULL, NULL, NULL}
};

PyMODINIT_FUNC
initpywhizard(void)
{
  (void) Py_InitModule("pywhizard", WhizardMethods);
  c_whizard_init(&w_instance);
}

int main(int argc, char *argv[])
{
  Py_SetProgramName(argv[0]);
  Py_Initialize;
  initpywhizard();
}
