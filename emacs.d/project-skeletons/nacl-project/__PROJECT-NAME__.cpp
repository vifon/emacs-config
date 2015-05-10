#include "ppapi/cpp/instance.h"
#include "ppapi/cpp/module.h"
#include "ppapi/cpp/var.h"

class __PROJECT-NAME__Instance : public pp::Instance
{
  public:
    explicit __PROJECT-NAME__Instance(PP_Instance instance)
        : pp::Instance(instance)
    {
    }

    virtual ~__PROJECT-NAME__Instance()
    {
    }

    virtual void HandleMessage(const pp::Var& var_message)
    {
        // TODO
    }
};

class __PROJECT-NAME__Module : public pp::Module
{
  public:
    __PROJECT-NAME__Module()
        : pp::Module()
    {
    }

    virtual ~__PROJECT-NAME__Module()
    {
    }

    virtual pp::Instance* CreateInstance(PP_Instance instance)
    {
        return new __PROJECT-NAME__Instance(instance);
    }
};

namespace pp {

Module* CreateModule()
{
    return new __PROJECT-NAME__Module();
}

} // namespace pp
