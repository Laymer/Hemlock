#include <errno.h>
#include <fcntl.h>
#include <stddef.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include <caml/mlvalues.h>

int flags_of_hemlock_file_flag[] = {
  /* R_O   */  O_RDONLY,
  /* W     */  O_WRONLY | O_CREAT,
  /* W_A   */  O_WRONLY | O_APPEND | O_CREAT,
  /* W_AO  */  O_WRONLY | O_APPEND,
  /* W_C   */  O_WRONLY | O_CREAT | O_EXCL,
  /* W_O   */  O_WRONLY,
  /* RW    */  O_RDWR | O_CREAT,
  /* RW_A  */  O_RDWR | O_APPEND| O_CREAT,
  /* RW_AO */  O_RDWR | O_APPEND,
  /* RW_C  */  O_RDWR | O_CREAT | O_EXCL,
  /* RW_O  */  O_RDWR,
};

uint8_t * deflate_bytes(size_t n, ssize_t * inflated_bytes) {
  uint8_t * deflated_bytes = (uint8_t *) inflated_bytes;
  for (size_t i = 0; i < n; i++) {
    deflated_bytes[i] = Int_val(inflated_bytes[i]);
  }
  deflated_bytes[n] = '\0';
  return deflated_bytes;
}

size_t * inflate_bytes(size_t n, uint8_t * deflated_bytes) {
  size_t * inflated_bytes = (size_t *) deflated_bytes;
  for (size_t i = n; i > 0; i--) {
    inflated_bytes[i - 1] = (size_t) Val_int(deflated_bytes[i - 1]);
  }
  return inflated_bytes;
} 

CAMLprim value
hemlock_file_print_byte(value a_byte) {
  uint8_t byte = (uint8_t) Val_int(a_byte);
  printf("Byte: %c; Value: %u\n", byte, byte);

  return Val_unit;
}

CAMLprim value
hemlock_file_print_bytes(value a_bytes, value a_n) {
  size_t* wide_bytes = (size_t *) String_val(a_bytes);
  size_t n = Int_val(a_n);
  char expected[] = "abcdefg\n";

  uint8_t * bytes = malloc(sizeof(uint8_t) * n + 1);
  for (size_t i = 0; i < n; i++) {
    bytes[i] = wide_bytes[i];
    printf("byte %i: %u %u %u\n", i, bytes[i], expected[i], wide_bytes[i]);
  }
  bytes[n + 1] = '\0';

  printf("printing bytes in c: %s\n", bytes);

  free(bytes);

  return Val_unit;
}

CAMLprim value
hemlock_file_error_to_string_get_length(value a_error) {
  int error = Int_val(a_error);

  int length = strlen(strerror(error));

  return Val_int(length);
}

CAMLprim value
hemlock_file_error_to_string_inner(value a_n, value a_bytes, value a_error) {
  size_t n = Long_val(a_n);
  size_t * bytes = (size_t *) String_val(a_bytes);
  int error = Int_val(a_error);

  uint8_t * cbytes = (uint8_t *) bytes;
  strncpy(cbytes, strerror(error), n);
  inflate_bytes(n, cbytes);

  return Val_unit;
}

CAMLprim value
hemlock_file_finalize_result(int result) {
  if (-1 == result) {
    result = -errno;
  }

  return Val_int(result);
}

CAMLprim value
hemlock_file_of_path_inner(value a_flag, value a_mode, value a_n, value a_bytes) {
  size_t flag = Long_val(a_flag);
  size_t mode = Long_val(a_mode);
  size_t * bytes = (size_t *) String_val(a_bytes);
  size_t n = Long_val(a_n);

  int flags = flags_of_hemlock_file_flag[flag];

  uint8_t * cbytes = deflate_bytes(n, bytes);
  // TODO expose mode in API
  int result = open(cbytes, flags, mode);
  inflate_bytes(n, cbytes);

  return hemlock_file_finalize_result(result);
}

CAMLprim value
hemlock_file_of_stdin_inner(value a_unit) {
  return hemlock_file_finalize_result(dup(STDIN_FILENO));
}

CAMLprim value
hemlock_file_of_stdout_inner(value a_unit) {
  return hemlock_file_finalize_result(dup(STDOUT_FILENO));
}

CAMLprim value
hemlock_file_of_stderr_inner(value a_unit) {
  return hemlock_file_finalize_result(dup(STDERR_FILENO));
}

CAMLprim value
hemlock_file_close_inner(value a_fd) {
  int fd = Int_val(a_fd);

  return hemlock_file_finalize_result(close(fd));
}

CAMLprim value
hemlock_file_read_inner(value a_n, value a_bytes, value a_fd) {
  size_t n = Long_val(a_n);
  size_t * bytes = (size_t *) String_val(a_bytes);
  int fd = Int_val(a_fd);

  uint8_t * cbytes = (uint8_t *) bytes;
  int result = read(fd, cbytes, n);
  if (result > 0) {
    inflate_bytes(result, cbytes);
  }

  return hemlock_file_finalize_result(result);
}

CAMLprim value
hemlock_file_write_inner(value a_i, value a_n, value a_bytes, value a_fd) {
  size_t i = Long_val(a_i);
  size_t n = Long_val(a_n);
  size_t * bytes = (size_t *) String_val(a_bytes);
  int fd = Int_val(a_fd);

  bytes = &bytes[i];
  uint8_t * cbytes = deflate_bytes(n, bytes);
  int result = write(fd, cbytes, n);
  inflate_bytes(n, cbytes);

  return hemlock_file_finalize_result(result);
}

CAMLprim value
hemlock_file_seek_inner(value a_i, value a_fd) {
  ssize_t i = Long_val(a_i);
  int fd = Int_val(a_fd);

  return hemlock_file_finalize_result(lseek(fd, i, SEEK_CUR));
}

CAMLprim value
hemlock_file_seek_hd_inner(value a_i, value a_fd) {
  size_t i = Long_val(a_i);
  int fd = Int_val(a_fd);

  return hemlock_file_finalize_result(lseek(fd, i, SEEK_SET));
}

CAMLprim value
hemlock_file_seek_tl_inner(value a_i, value a_fd) {
  size_t i = Long_val(a_i);
  int fd = Int_val(a_fd);

  return hemlock_file_finalize_result(lseek(fd, i, SEEK_END));
}


/*
// val hemlock_file_of_path_inner: File.Mode.t -> Bytes.t -> isize
CAMLprim value
hemlock_file_error_to_string_inner(value a_error) {
  int error = Long_val(a_error);

  const char* bytes = sterror(error);

  printf("error %s\n", error);

  return Val_(bytes);
}
*/