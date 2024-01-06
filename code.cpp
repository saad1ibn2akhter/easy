/*#include<bits/stdc++.h>
using namespace std;

int prime(int n)
{
    if(n == 2)return 0;

    if(n<1)return 1;
    for(int i =2;i<n;i++){
        if(n%i == 0){
            return 1;
        }
    }
    return 1;
}

int main()
{
    return 0;
}

#include<bits/stdc++.h>
using namespace std;

int prime(int n){
    if(n == 2)return 0;

    if(n<1)return 1;
    for(int i =2;i<n;i++){
        if(n%i == 0){
            return 1;
        }
    }
    return 1;
}


int main()
{
    string str;
    cin>>str;
    int count_cap =0,count_sml =0;
    int len= str.size();
    for(int i =0;i<len;i++){
        if(str[i]>=65 && str[i]<97){
            count_cap++;
        }
        else if(str[i]>=97 && str[i]<=122){
            count_sml++;
        }
    }
    if(count_cap< count_sml || count_cap == count_sml){
        for(int i =0;i<len;i++){
            if(str[i]>=65 && str[i]<97){
                cout<<str[i]+32;
            }
            else if(str[i]>=97 && str[i]<=122){
                cout<<str[i];               
            }
        }
    }
    else if(count_cap > count_sml){
        for(int i  =0;i<len;i++){
            if(str[i]>=65 && str[i]<97){
                cout<<str[i];
            }
            else if(str[i]>=97 && str[i]<=122){
                cout<<str[i]-32;

            }
        }
    }
    return 0;
}

#include<bits/stdc++.h>
using namespace std;

int main()
{
    vector<int>vec;
    int n,m;
    cin>>n;
    for(int i =0;i<n;i++){
        cin>>m;
        vec.push_back(m);
    } 
    for(int i = 0;i<vec.size();i++){
        for(int j =vec[i];j<vec.size()-i;j++){
            if(vec[j]==vec[i])//its wrong maybe
            {
                vec.erase(vec[j])//delete....i think this func will not work
            }
        }
    }
    cout<<vec.size();
    for(auto x:vec){
        cout<<x<<" ";
    }cout<<endl;   
    return 0;
}

#include<bits/stdc++.h>
using namespace std;

int main()
{
    vector<int>vec;
    vector<int>vec2;
char array[26] = {"a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v",
"w","x","y","z"
};
for(int m =0;m<26;m++){
    vec.push_back(m[i]);
}
int num_array[26] = {1,2,3,4,5,6,7,8,9,100,110,120,130,140,150,160,170,180,190,200,210
,220,230,240,250,260} ;
for(int n =0;n<26;n++){
    vec2.push_back(n[i]);
}
string s;
cin>>s;
for(int i =0;i<s.size();i++){
    for(int j =0;j<vec
    .size();j++){
        if(s[i]==vec[j]){
            cout<<vec2[j];
        }
    }
}
return 0;

}

#include<iostream>
using namespace std;

int main()
{
    cout<<"Durbar Bangla"<<"\n"<<endl;
    return 0;
}

#include<bits/stdc++.h>
using namespace std;

int main()
{
    string str;
    int n;

    cin>>n;
    for(int i = 0;i<n;i++){
        cin>>str[i];
    }
    for(int i =0;i<n;i++){
        if(str[i]=='8'){
            for(int j = i;j<=0;j--){
                str.erase();
            }
        }
    }
    while(str.size()==11){
        str.pop_back();
    }
    if(str.size()==11)cout<<"yes";
    if(str.size()<11)cout<<"no";
    return 0;
}

#include<bits/stdc++.h>
using namespace std;

int main()
{
int row,col;    
cin>>col>>row;
int array[row][col];
for ( i = 0; i < row; i++)
{
    for ( j = 0; i < col; j++)
    {
        cin>>array[row][col];
    }
    cout<<"\n";
}
for(int n =0;n<row)

}


#include<bits/stdc++.h>
using namespace std;

int main()
{
int a,b,val;
cin>>a>>b;
vector<int>vec;
vector<int>vec2;
for(int i =1;i<50;i++){
    int num = a*3;
    vec.push_back(num);
}
for(int j =1;j<50;j++){
    int num2 = b*2;
    vec2.push_back(num2);
}
for(int n =0,m=0;n<vec.size(),m<vec2.size();n++,m++){
    if(vec[n]>vec[m]){
        val = n+1;
    }
}
cout<<val<<"\n"<<endl;
return 0;

}


#include<bits/stdc++.h>
using namespace std;

int main()
{
    int test;
    vector<string>vec;
    string str;
    cin>>test;
    while(test--){
        int len,num;
        cin>>len>>num;
        while(len--){
            cin>>str;
            vec.push_back(str);
        }
        vec.push_back(num);
        sort(vec.begin(),vec.end());
        reverse(vec.begin(),vec.end());
        for(auto x:vec){
            cout<<x;
        }cout<<endl;
    }
    return 0;
}

#include<bits/stdc++.h>
using namespace std;

int main()
{
    int test;
    long long int pro = 1;
    cin>>test;
    while(test--){
        int len;
        int num;
        vector<int>vec;
        cin>>len;
        while(len--){
            cin>>num;
            vec.push_back(num);
        }
        sort(vec.begin(),vec.end());
        reverse(vec.begin(),vec.end());
        vec[vec.size()-1]+1;
        for(int i =0;i<vec.size();i++){
            pro*=vec[i]; 
        }
        cout<<pro<<endl;


    }
    return 0;
}


#include<bits/stdc++.h>
using namespace std;

int main()
{
    int test;
    string str;
    cin>>test;
    while(test--){
        cin>>str;
        int len = str.size()/2;
        while(str.size()==len){
            str.pop_back();
        }
        reverse(str.begin(),str.end());
        cout<<str<<"\n";
        }
        return 0;
}


#include<bits/stdc++.h>
using namespace std;

int main()
{
    int test,digits;
    vector<int>vec;
    cin>>test;
    while(test--){
        int len,digit;
        int num;
        cin>>len>>digit;
        cin>>num;
        while(num<0){
            digits=num%10;
            vec.push_back(digits);
            num/=10;

        }
        vec.push_back(digit);
        sort(vec.begin(),vec.end());
        reverse(vec.begin(),vec.end());
        for(auto x:vec){
            cout<<x;
        }cout<<endl;

    }
    return 0;
}
// Online C++ compiler to run C++ program online
#include <bits/stdc++.h>
using namespace std;

int prime(int num){
    for(int i =2;i<num;i++){
        if(num%i==0){
            return false;
        }
    }
    return true;
}

int main() {
    int test,a,b;
    vector<int>vec;
    cin>>test;
    while(test--){
        cin>>a>>b;
                if(b<=1){
            cout<<"-1"<<"\n";
            continue;            
        }
        if(b==2){
            cout<<b<<"\n";
            continue;
        }

        for(int i = a;i<=b;i++){
            
            if(prime(i)==true){vec.push_back(i);}
        }
        if(vec.size()==0){
            cout<<"-1"<<"\n";
            continue;
        }
        cout<<vec[vec.size()-1]<<"\n";
    }

    return 0;
}
// Online C++ compiler to run C++ program online
#include <bits/stdc++.h>
using namespace std;

int main() {
    int test;
    cin>>test;
    while(test--){
        string str;
        cin>>str;
        int len = str.size();
        if(str[len-1]=='H' &&str[len-2]=='O' &&str[len-3]=='O' &&str[len-4]=='C' ){
            cout<<"Carboxylic Acid"<<"\n";
        }
        else if(str[len-1]=='H' &&str[len-2]=='O' &&str[len-3]!='O' &&str[len-4]!='C' ){
            cout<<"Alcohol"<<"\n";
        }
        else{
            cout<<"Alkane"<<"\n";
        }
    }

    return 0;
}
#include<bits/stdc++.h>
using namespace std;

int main()
{
    int test,digits;
            int len,digit;
        int num;
    vector<int>vec;
    cin>>test;
    while(test--){

        cin>>len>>digit;
        cin>>num;
        //cin.ignore();
         if(num == 0)vec.push_back(0);
        while(num>0){
            digits=num%10;
            vec.push_back(digits);
            num/=10;

        }
       
        vec.push_back(digit);
        sort(vec.begin(),vec.end());
        reverse(vec.begin(),vec.end());
        for(auto x:vec){
            cout<<x;
        }cout<<"\n"<<endl;
        vec.clear();
        

    }
    //return 0;
}
*/
#include <iostream>
using namespace std;

int  solve(int n,int m){
    int  answer =0;
    for(int x =1;x<=m;x++){
        bool valid = true;
        for(int y =1;y<x&&valid;y++){
            for(int z =1;z<n&&valid;z++){
                if((x*z) == (y*n)){
                    valid =false;
                }
            }
        }
        if(valid){
            answer++;
        }
    }
    return answer;
}

int main() {
    int test,n,m;
    cin>>test;
    for(int i =1;i<=test;i++){
        cin>>n>>m;
        int result =  solve(n,m);
        cout<<"Case "<<test<<" :"<<result<<endl;
    }
    return 0;
}

#include <bits/stdc++.h>
using namespace std;

int main() {
    string str;
    int test;
    cin>>test;
    while(test--){
        cin>>str;
        int answer = str[str.size()-1]-48;
        if(answer<4){
            cout<<answer*answer<<endl;
        }
        else{
            answer = (answer*answer)%10;
            cout<<answer<<endl;
        }
    }
    return 0;
}



#include<bits/stdc++.h>
using namespace std;

int main(){
    string str;
    cin>>str;
    string konami ="UUDDLRLRBA";
    int ans = str.find(konami);
    int pro_ans =0;
    while(ans != -1){
        pro_ans++;
        ans = str.find(konami,ans+1);
    }
    cout<<pro_ans<<endl;
    return 0;
}


https://shorturl.at/afF69
944 656 9001
Dreamers

// Online C++ compiler to run C++ program online
#include <bits/stdc++.h>
using namespace std;

int main() {
    int t1,t2,num,i,j;
    cin>>t1>>t2;
    vector<int>vec;
    while(t1--){
        cin>>num;
        vec.push_back(num);
    }
    
    while(t2--){
        cin>>num;
        vec.push_back(num);
    }
    sort(vec.begin(),vec.end());
    
    for(i = 0,j =1;i<vec.size()-2,j<vec.size()-1;i++,j++){
        if(vec[i] == vec[j]){
            vec.erase(vec.begin()+j);
        }
    }
    
    for(auto x : vec){
        cout<<x<<" ";
    }cout<<endl;
    return 0;
}