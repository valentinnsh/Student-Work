/*
 * Copyright (c) 2018 V.Shishkin
 *
 */

#include <iostream>
#include <stack>
#include <string>

using std::stack;
using std::string;

class Automate_c{
  stack<string> m_buffer;
  int m_curr_state;
public:
  Automate_c(){
    m_buffer.push("Z_0");
    m_curr_state = 0;
  }

  bool check_chain(string chain){
    int check = -1;
    for(unsigned int i = 0; i<=chain.size(); ++i){
      if(m_buffer.empty())
        throw "wrong symbol "+ chain.substr(i-1,1) +" at pos " +
          std::to_string(i-1) + ".\nUnexpected symbol after the  end of the accepttable chain";
      string top = m_buffer.top();
      m_buffer.pop();
      check = this->change_state(top,chain.substr(i,1), i);
    }
    if(check == -4)
      return true;
    else return false;
  }

  int change_state(string &top,string symb, unsigned int pos){
    //-----------------------Q0-----------------
    if (m_curr_state == 0){
      if(top == "Z_0"){
        //(q_0,a,Z_0)
        if(symb == "a"){
          m_curr_state = 1;
          m_buffer.push("Z_0");
          m_buffer.push("A");
          return 1;
        }else if(symb == "0"){
          //(q_0,0,Z_0)
          m_curr_state = 2;
          m_buffer.push("Z_0");
          return 2;
        }else throw "character " + symb + " at pos " + std::to_string(pos) + " is not allowed.\nExpected a or 0\n";
      }else if(top == "A"){
        //(q_0,a,A)
        if(symb == "a"){
          m_curr_state = 1;
          m_buffer.push("A");
          m_buffer.push("A");
          return 1;
        }else if(symb == "0"){
          //(q_0,0,A)
          m_curr_state = 2;
          m_buffer.push("A");
          return 2;
        }else throw "character " + symb + " at pos " + std::to_string(pos) + " is not allowed.\nExpected a or 0\n";   
      }
    }else if(m_curr_state == 1){
      //-------------------Q1--------------------
      if((top=="A")&&(symb=="a")){
        //(q_1,a,A)
        m_curr_state = 0;
        m_buffer.push("A");
        return 0;
      }else throw "character " + symb + " at pos " + std::to_string(pos) + " is not allowed.\nExpected a\n";
    }else if(m_curr_state == 2){
//---------------------------Q2--------------------------------
      if(top=="A"){
        if(symb =="b"){
          //(q_2,b,A) Start b^n
          m_curr_state = 2;
          m_buffer.push("A");
          m_buffer.push("B");
          return 2;
        }else if(symb == "1"){
          //(q_2,1,A) Skip b^n
          m_curr_state = 3;
          m_buffer.push("A");
          return 3;
        }else throw "character " + symb + " at pos " + std::to_string(pos) + " is not allowed.\nExpected b or 1\n";
      }else if(top == "Z_0"){
        if(symb =="b"){
          //(q_2,b,Z_0)
          m_curr_state = 2;
          m_buffer.push("Z_0");
          m_buffer.push("B");
          return 2;
        }else if(symb == "1"){
          //(q_2,1,Z_0) Skip b^n
          m_curr_state = 3;
          m_buffer.push("Z_0");
          return 3;
        }else throw "character " + symb + " at pos " + std::to_string(pos) + " is not allowed.\nExpected b or 1\n";
      }else if(top == "B"){
        if(symb =="b"){
          //(q_2,b,B)
          m_curr_state = 2;
          m_buffer.push("B");
          m_buffer.push("B");
          return 2;
        }else if(symb == "1"){
          //(q_2,1,B) b^n was not skipped
          m_curr_state = 3;
          m_buffer.push("B");
          return 3;
        }else throw "character " + symb + " at pos " + std::to_string(pos) + " is not allowed.\nExpected b or 1\n";
      }
    }else if(m_curr_state == 3){
//----------------------------Q3----------
      if(top == "B"){
        if(symb == "b"){
          //(q_3,b,B)
          m_curr_state = 3;
          return 3;
        }else throw "character " + symb + " at pos " + std::to_string(pos) + " is not allowed.\nExpected b\n";
      }else if(top == "Z_0"){
        if(symb == "0"){
          //(q_3,0,Z_0)
          m_curr_state = 4;
          m_buffer.push("Z_0");
          return 4;
        }else throw "character " + symb + " at pos " + std::to_string(pos) + " is not allowed.\nExpected 0\n";
      }else if(top == "A"){
        if(symb == "0"){
          //(q_3,0,A)
          m_curr_state = 4;
          m_buffer.push("A");
          return 4;
        }else throw "character " + symb + " at pos " + std::to_string(pos) + " is not allowed.\nExpected 0\n";
      }
      std::cout << m_curr_state<<"ok\n";
    }else if(m_curr_state == 4){
//----------------------------Q4------------
      if(top == "Z_0"){
        return -4;
      }else if(top == "A"){
        if(symb == "a"){
          //(q_4,a,A)
          m_curr_state = 4;
          return 4;
        }else if(symb == ""){
          throw "Expexted a, but found end of the chain at pos " + std::to_string(pos); 
        }else throw "character " + symb + " at pos " + std::to_string(pos) + " is not allowed.\nExpected a\n";
      }
    }
    return -2;
  }
};

int main(int argc, char* argv[]){
  (void)argc;(void)argv;
  Automate_c working_automate;
  string chain;
  std::cout << "Enter a chain: ";
  std::cin >> chain;
  try{
    if(working_automate.check_chain(chain))
      std::cout << "Accepttable chain\n";
  }catch(string error){
    std::cout << "ERROR: " << error << "\n";
  }

  return 0;
}
