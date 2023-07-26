module brownianmotion
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, brownianmotion!"
  end subroutine say_hello
end module brownianmotion
