/*
Copyright © 2016 by kenny
All Right Reserved.
*/

#include<bits/stdc++.h>
#define rep(i, j, k) for(int i = j;i <= k;i++)
#define repm(i, j, k) for(int i = j;i >= k;i--)
#define mem(a) memset(a, 0, sizeof(a))
using namespace std;
const double eps = 1e-8;
const double pi = acos(-1);

const double EPS = 1e-12;
const double inf = 1e12;


inline int sign(double x) {
	return x < -EPS ? -1 : x > EPS;
}
//get the value of a polynomial(the variable equals to x)
inline double get(const vector<double> &coef, double x) {
	double e = 1 , s = 0;
	rep(i, 0, coef.size() - 1) s += e * coef[i], e *= x;
	return s;
}
//get the equation's root between lo & hi
double find(const vector<double> &coef, int n, double lo, double hi) {
	int sign_lo, sign_hi;
	if((sign_lo = sign(get(coef, lo))) == 0) return lo;
	if((sign_hi = sign(get(coef, hi))) == 0) return hi;
	if(sign_lo * sign_hi > 0) return inf;
	for(int step = 0; step < 100 && hi - lo > EPS; ++step) {
		double mid = (lo + hi) * 0.5;
		int sign_mid = sign(get(coef, mid));
		if(sign_mid == 0) return mid;
		if(sign_mid * sign_lo > 0) lo = mid;
		else hi = mid;
	}
	return (lo + hi) * 0.5;
}
//get all roots of the equation
vector<double> solve(vector<double> coef, int n) {
	vector<double> ret;
	if(n == 1) {
		if(sign(coef[1])) ret.push_back(-coef[0] / coef[1]);
		return ret;
	}
	vector<double> dcoef(n);
	rep(i, 0, n-1) dcoef[i] = coef[i+1] * (i + 1);
	vector<double> droot = solve(dcoef, n - 1);
	droot.insert(droot.begin(), -inf);
	droot.push_back(inf);
	for(int i = 0; i + 1 < droot.size(); ++i) {
		double tmp = find(coef, n, droot[i], droot[i + 1]);
		if(tmp < inf) ret.push_back(tmp);
	}
	return ret;
}


//a class of polynomial
struct poly{
	int n;
	double num[210];
	void clear() {
		rep(i, 0, 209) num[i] = 0;
	}
	poly() {}
	//using a vactor to generate a polynomial
	poly(vector<double> vec): n(vec.size() - 1) {
		clear();
		rep(i, 0, vec.size() - 1)
		num[i] = vec[i];
	}
	void print() {
		cout << endl;
		cout << num[n] << "x^" << n;
		repm(i, n-1, 0)
		{
			if(num[i] >= 0)
			cout << "+" << num[i] << "x^" << i;
			else
			cout << num[i] << "x^" << i;
		}
		cout << endl;
	}
	//return the addition of two polynomials
	friend poly operator + (poly a, poly b) {
		poly ans;
		ans.n = max(a.n, b.n);
		ans.clear();
		rep(i, 0, ans.n)
		ans.num[i] = a.num[i] + b.num[i];
		while(abs(ans.num[ans.n]) < eps && ans.n > 0)
		ans.n--;
		return ans;
	}
	//return the product of a polynomial and a number
	friend poly operator * (poly a, double b) {
		poly ans = a;
		rep(i, 0, ans.n) ans.num[i] *= b;
		ans.n = 200;
		while(abs(ans.num[ans.n]) < eps && ans.n > 0)
		ans.n--;
		return ans;
	}
	//return the subtraction of two polynomials
	friend poly operator - (poly a, poly b) {
		return a + (b * -1.0);
	}
	//return the product of two polynomials
	friend poly operator * (poly a, poly b) {
		poly ans;
		ans.n = 200;
		ans.clear();
		rep(i, 0, a.n) rep(j, 0, b.n)
		ans.num[i + j] += a.num[i] * b.num[j];
		while(abs(ans.num[ans.n]) < eps && ans.n > 0)
		ans.n--;
		return ans;
	}
	//return the ratio of two polynomials
	friend poly operator / (poly a, poly b) {
		poly ans;
		ans.n = 200;
		ans.clear();
		while(a.n || abs(a.num[0]) > eps)
		{
			int dif = a.n - b.n;
			vector<double> vec;
			rep(i, 1, dif)
			vec.push_back(0);
			vec.push_back(a.num[a.n] / b.num[b.n]);
			poly part = poly(vec);
			a = a - (part * b);
			ans = ans + part;
		}
		while(abs(ans.num[ans.n]) < eps&&ans.n > 0)
		ans.n--;
		return ans;
	}
	bool empty() {
		if(n == 0 && abs(num[0]) < eps)
		return 1;
		return 0;
	}
	//return if a can be devided by b
	friend bool can_be_devided (poly a, poly b) {
		while(a.n >= b.n && !a.empty())
		{
			int dif = a.n - b.n;
			vector<double> vec;
			rep(i, 1, dif)
			vec.push_back(0);
			vec.push_back(a.num[a.n] / b.num[b.n]);
			poly part = poly(vec);
			a = a - (part * b);
		}
		if(!a.empty())
		return 0;
		return 1;
	}
	//get the roots of the equation(polynomial==0)
	vector<double> solu() {
		vector<double> vec;
		rep(i, 0, n) vec.push_back(num[i]);
		return solve(vec, n);
	}
};


//a class of matrix; the elements are reals
struct matrix{
	int n, m;
	double num[101][101];
	//scan the arguments & elements
	void scan(){
		cin >> n >> m;
		rep(i, 1, n) rep(j, 1, m)
		cin >> num[i][j];
	}
	//fill the matrix with element 0
	void clear(){
		rep(i, 1, 100) rep(j, 1, 100) num[i][j] = 0;
	}
	//let the matrix equals to I(or E)
	void normalize(){
		clear();
		rep(i, 1, n)
		num[i][i] = 1.0;
	}
	matrix () { clear(); }
	//print the arguments and elements
	void print(){
		cout << endl;
		if(n == 0 || m == 0) {
			cout << "matrix is empty" << endl;
			return;
		}
		cout << n  << " " << m << endl;
		rep(i, 1, n) {
			rep(j, 1, m)
			if(abs(num[i][j]) > eps)
			cout << num[i][j] << " ";
			else
			cout << "0 ";
			cout << endl;
		}
	}
	//swap the two lines of the matrix
	void swap_hang(int i, int j) {
		rep(cnt, 1, m)
		swap(num[i][cnt], num[j][cnt]);
	}
	//swap the two rows of the matrix
	void swap_lie(int i, int j) {
		rep(cnt, 1, n)
		swap(num[cnt][i], num[cnt][j]);
	}
	//update the j(th) line with the value of line(j)-mul*line(i)
	void minus_hang(int i, int j, double mul) {
		rep(cnt, 1, m)
		num[j][cnt] -= num[i][cnt] * mul;
	}
	//update the j(th) row with the value of row(j)-mul*row(i)
	void minus_lie(int i, int j, double mul){
		rep(cnt, 1, n)
		num[cnt][j] -= num[cnt][i] * mul;
	}
	//update the i(th) line with the value of mul*line(i)
	void mul_hang(int i, double mul) {
		rep(cnt, 1, m)
		num[i][cnt] *= mul;
	}
	//update the i(th) row with the value of mul*row(i)
	void mul_lie(int i, double mul) {
		rep(cnt, 1, n)
		num[cnt][i] *= mul;
	}
	//return the addition of two matrixs
	matrix operator + (matrix b){
		matrix x = (*this);
		if(n == b.n && m == b.m)
		rep(i, 1, n)
		rep(j, 1, m)
		x.num[i][j] += b.num[i][j];
		cout << "can't add, will return the first matrix" << endl;
		return x;
	}
	//return the subtraction of two matrixs
	matrix operator - (matrix b){
		matrix x = (*this);
		if(n == b.n && m == b.m)
		rep(i, 1, n)
		rep(j, 1, m)
		x.num[i][j] -= b.num[i][j];
		else
		cout << "can't subtract, will return the first matrix" << endl;
		return x;
	}
	//return the product of two matrixs
	matrix operator * (matrix b){
		matrix x = (*this);
		if(m == b.n) {
			x.n = n;
			x.m = b.m;
			x.clear();
			rep(i, 1, x.n)
			rep(j, 1, x.m)
			rep(k, 1, m)
			x.num[i][k] += num[i][j] * b.num[j][k];
		}
		else
		cout << "can't multiply, will return the first matrix" << endl;
		return x;
	}
	//return the product of a matrix and a number
	friend matrix operator *(matrix a, double b) {
		rep(i, 1, a.n) rep(j, 1, a.m)
		a.num[i][j] *= b;
		return a;
	}
	//return the transposition matrix of the original matrix
	matrix transposition(){
		matrix a;
		a.m = n, a.n = m;
		rep(i, 1, n) rep(j, 1, m)
		a.num[j][i] = num[i][j];
		return a;
	}
	//return the value of a determinate
	double val(){
		int tot = 0;
		matrix tmp = (*this);
		if(n != m) {
			cout << "can't get the value of rectangle matrix, will return 0.0" << endl;
			return 0;
		}
		rep(i, 1, n) {
			if(abs(num[i][i]) < eps) {
				int cnt = i + 1;
				while(abs(num[cnt][i]) < eps && cnt <= n)
				cnt++;
				if(cnt == n + 1)
				return 0;
				else {
					swap_hang(i, cnt);
					tot++;
				}
			}
			rep(j, i + 1, n) {
				double cst = num[j][i] / num[i][i];
				minus_lie(i, j, cst);
			}
		}
		double ans = 1;
		rep(i, 1, n)
		ans *= num[i][i];
		*this = tmp;
		if(tot % 2)
		return -ans;
		return ans;
	}
	//return the submatrix of the matrix
	matrix submatrix(int ver1, int ver2, int hor1, int hor2){
		matrix ans;
		ans.clear();
		ans.n = hor2 - hor1 + 1; ans.m = ver2 - ver1 + 1;
		rep(i, 1, ans.n)
		rep(j, 1, ans.m)
		ans.num[i][j] = num[hor1 + i - 1][ver1 + j - 1];
		return ans;
	}
	//if the solution is unique, return 1 and change the original n*(n+1) matrix into its solution
	//else return 0
	bool la_solution1(){
		if(abs(submatrix(1, n, 1, n).val()) < eps)
		return 0;
		rep(i, 1, n) {
			if(abs(num[i][i]) < eps) {
				int cnt = i + 1;
				while(abs(num[cnt][i]) < eps && cnt <= n)
				cnt++;
				swap_hang(i, cnt);
			}
			rep(j, i + 1, n) {
				double cst = num[j][i] / num[i][i];
				minus_hang(i, j, cst);
			}
		}
		rep(i, 1, n)
		mul_hang(i, 1.0 / num[i][i]);
		repm(i, n, 2) {
			rep(j, 1, i - 1)
			minus_hang(i, j, num[j][i]);
		}
		return 1;
	}
	//convert the matrix into row echelon form
	matrix reduced(){
		matrix x = (*this);
		int tot_cnt = 0;
		rep(i, 1, n) {
			if(i + tot_cnt > m)
			break;
			if(abs(x.num[i][i+tot_cnt]) < eps) {
				int cnt = -1;
				rep(j, i + 1, x.n) {
					if(abs(x.num[j][i+tot_cnt]) > eps)
					cnt = j;
				}
				if(cnt == -1) {
					tot_cnt++;
					i--;
					continue;
				} else {
					x.swap_hang(i, cnt);
				}
			}
			rep(j, i + 1, n) {
				double cst = x.num[j][i + tot_cnt] / x.num[i][i + tot_cnt];
				x.minus_hang(i, j, cst);
			}
		}
		return x;
	}
	//convert the matrix into simplest form(only used for get the reverse)
	matrix normal_reduced()	{
		matrix x = (*this).reduced();
		rep(i, 1, n)	{
			double tmp = x.num[i][i];
			x.mul_hang(i, 1.0 / tmp);
		}
		repm(i, n, 2) {
			rep(j, 1, i - 1)
			x.minus_hang(i, j, x.num[j][i]);
		}
		return x;
	}
	//return the rank of the matrix
	int rank() {
		matrix x = (*this).reduced();
		int i;
		for(i = 1; i <= x.n; i++)
		{
			int flag = 1;
			rep(j, 1, x.m)
			{
				if(abs(x.num[i][j]) > eps)
				flag = 0;
			}
			if(flag)
			break;
		}
		return i - 1;
	}
	//return the reverse of the matrix
	matrix reverse() {
		matrix x;
		if(abs(val()) < eps)
		{
			x = (*this);
			cout << "rank<(n-1),no reverse,will return the matrix" << endl;
		}
		x.clear();
		x.n = n, x.m = 2 * n;
		rep(i, 1, n) rep(j, 1, n) x.num[i][j] = num[i][j];
		rep(i, 1, n) x.num[i][n + i] = 1;
		x = x.normal_reduced();
		x.print();
		return x.submatrix(n + 1, 2 * n, 1, n);
	}
	//0 return the congruent standard form of the matrix B
	//1 return the matrix A, s.t. A.transposition*C*A==B (C is the original matrix)
	matrix normal_bi(bool flag) {
		matrix x;
		x.n = 2 * n;
		x.m = n;
		x.clear();
		rep(i, 1, n) rep(j, 1, n) x.num[i][j] = num[i][j];
		rep(i, 1, n) x.num[n + i][i] = 1;
		rep(i, 1, n - 1)
		{
			x.print();
			if(abs(x.num[i][i]) < eps)
			{
				rep(j, i + 1, n)
				if(abs(x.num[j][i]) > eps)
				{
					x.minus_hang(j, i, -1);
					x.minus_lie(j, i, -1);
					break;
				}
			}
			if(abs(x.num[i][i]) < eps)
			{
				continue;
			}
			rep(j, i + 1, n)
			{
				double mul = x.num[j][i] / x.num[i][i];
				x.minus_hang(i, j, mul);
				x.minus_lie(i, j, mul);
			}
		}
		if(flag)
		return x.submatrix(1, n, 1, n);
		else
		return x.submatrix(1, n, 1 + n, 2 * n);
	}
	/*
	//Jacobi method
	matrix diag() {
		matrix xx=(*this);
		double mx=0;
		do {
			int mi=0,mj=0;
			mx=0;
			rep(i,1,n) rep(j,1,n)
			{
				if(i!=j&&abs(num[i][j])>mx)
				{
					mx=abs(num[i][j]);
					mi=i;mj=j;
				}
			}
			if(mi>mj)
			swap(mi,mj);
			double ang;
			if(num[mi][mi]!=num[mj][mj])
			ang=atan(-2.0*num[mi][mj]/(num[mi][mi]-num[mj][mj]));
			else
			{
				if(num[mi][mj]>0)
				ang=-pi/2;
				else
				ang=pi/2;
			}
			ang/=2.0;
			matrix tmp;
			tmp.m=tmp.n=n;
			tmp.normalize();
			tmp.num[mi][mi]=tmp.num[mj][mj]=cos(ang);
			tmp.num[mj][mi]=sin(ang);
			tmp.num[mi][mj]=-sin(ang);
			(*this)=tmp.transposition()*(*this)*tmp;
		}while(mx>eps);
		swap(*this,xx);
		return xx;
	}
	*/
};


//a class of matrix; the elements are polynomials
struct poly_matrix{
	int n, m;
	poly x[11][11];
	poly_matrix() {}
	//generate a determinate, which value is the feature polynomial of the matrix orig
	poly_matrix(matrix orig): n(orig.n) {
		rep(i, 1, n) rep(j, 1, n)
		{
			vector<double> vec;
			vec.push_back(-orig.num[i][j]);
			if(i == j)
			vec.push_back(1.0);
			x[i][j] = poly(vec);
		}
	}
	//print the poly_matrix
	void print() {
		cout << endl;
		rep(i, 1, n) {
			rep(j, 1, n)
			x[i][j].print();
			cout << endl;
		}
		cout << endl;
	}
	//swap the 2 lines of the poly_matrix
	void _swap(int ii, int jj) {
		rep(i, 1, n)
		swap(x[ii][i], x[jj][i]);
	}
	//update the jj(th) line with the value of line(jj)-fac*line(ii)
	void _sub(int ii, int jj, poly fac) {
		rep(i, 1, n)
		x[jj][i] = x[jj][i] - x[ii][i] * fac;
	}
	//update the ii(th) line with the value of line(ii)*fac
	void _mult(int ii, poly fac) {
		rep(i, 1, n)
		x[ii][i] = x[ii][i] * fac;
	}
	//return the value(a polynomial) of the poly_matrix
	poly feature_poly() {
		vector<double> tmp;
		tmp.push_back(1);
		poly_matrix ttmp = (*this);
		poly factor = poly(tmp);
		poly final = factor;
		rep(i, 1, n - 1) {
			int maxlen = 1000;
			int id = 0;
			rep(j, i, n) {
				if(!x[j][i].empty() && x[j][i].n < maxlen) {
					maxlen = x[j][i].n;
					id = j;
				}
			}
			if(id == 0) {
				vector<double> vec;
				vec.push_back(0.0);
				return poly(vec);
			}
			if(id != i) {
				factor = factor * -1.0;
				_swap(i, id);
			}
			rep(j, i + 1, n) {
				if(can_be_devided(x[j][i], x[i][i]))
				_sub(i, j, x[j][i] / x[i][i]);
				else {
					_mult(j, x[i][i]);
					factor = factor * x[i][i];
					_sub(i, j, x[j][i] / x[i][i]);
				}
			}
		}
		rep(i, 1, n)
		final = final * x[i][i];
		final = final / factor;
		(*this) = ttmp;
		return final;
	}
};


//a class of vector
struct vct{
	int n;
	double num[101];
	vct() {clear();}
	//set the ii(th) row as the vector's value
	vct(matrix x, int ii): n(x.n) {
		rep(i, 1, x.n) num[i] = x.num[i][ii]; clear();
	}
	//return the dot of 2 vectors
	double friend operator * (vct a, vct b) {
		double ans = 0;
		rep(i, 1, a.n) ans += a.num[i] * b.num[i];
		return ans;
	}
	//return the product of a vector and a number
	vct friend operator * (vct a, double b) {
		rep(i, 1, a.n) a.num[i] *= b;
		return a;
	}
	//return the addition of 2 vectors
	vct friend operator + (vct a, vct b) {
		rep(i, 1, a.n) a.num[i] += b.num[i];
		return a;
	}
	//return the subtraction of 2 vectors
	vct friend operator - (vct a, vct b) {
		rep(i, 1, a.n) a.num[i] -= b.num[i];
		return a;
	}
	//convert the vector into unitization form
	void normalize() {
		double sum = 0;
		rep(i, 1, n) sum += num[i] * num[i];
		sum = sqrt(sum);
		rep(i, 1, n) num[i] /= sum;
	}
	//print the vector
	void print() {
		cout << endl;
		rep(i, 1, n)
		cout << num[i] << " ";
		cout << endl;
	}
	//clear the vector
	void clear() {
		rep(i, 0, 100) num[i] = 0;
	}
};


//save the vector as the ii(th) row of the matrix
void save(matrix &x, int ii, vct v) {
	rep(i, 1, v.n)
	x.num[i][ii] = v.num[i];
}

bool cmp(double a,double b) {
	return abs(a) > abs(b);
}
//get the U, s.t. U^-1 * orig * U = diag, U is saved in as
bool find_T(matrix x, matrix &as) {
	as.m = as.n = x.n;
	poly_matrix tmp = poly_matrix(x);
	poly p_tmp = tmp.feature_poly();
	vector<double> vec = p_tmp.solu();
	sort(vec.begin(), vec.end(), cmp);
	vector<double> feature;
	feature.push_back(vec[0]);
	rep(i, 1, vec.size() - 1) {
		if(abs(vec[i] - vec[i - 1]) > eps)
		feature.push_back(vec[i]);
	}
	vec = feature;
	matrix I;
	I.m = I.n = x.n;
	I.normalize();
	vector<vct> vecc;
	rep(i, 0, vec.size() - 1) {
		matrix mat = I * vec[i] - x;
		mat = mat.reduced();
		if(mat.rank() == 0) {
			rep(ii, 1, mat.n) {
				vct tmp;
				tmp.n = mat.n;
				tmp.num[ii] = 1.0;
				vecc.push_back(tmp);
			}
			continue;
		}
		bool main[101];
		mem(main);
		rep(ii, 1, mat.rank()) {
			rep(j, 1, mat.m) {
				if(abs(mat.num[ii][j]) > eps) {
					main[j] = 1;
					break;
				}
			}
		}
		rep(ii, 1, mat.n) {
			if(!main[ii]) {
				matrix tmp;
				tmp.m = tmp.n = mat.rank();
				int cnt = 0;
				rep(ti, 1, mat.n) {
					if(main[ti]) {
						cnt++;
						rep(j, 1, mat.rank())
						tmp.num[j][cnt] = mat.num[j][ti];
					}
				}
				tmp.m++;
				rep(j, 1, mat.rank())
				tmp.num[j][tmp.m] = -mat.num[j][ii];
				tmp.la_solution1();
				vct ans;
				ans.n = mat.n;
				cnt = 0;
				rep(j, 1, mat.n) {
					if(main[j]) {
						cnt++;
						ans.num[j] = tmp.num[cnt][tmp.m];
					}
				}
				ans.num[ii] = 1.0;
				vecc.push_back(ans);
			}
		}
	}
	if(vecc.size() < x.n)
	return 0;
	else {
		rep(i, 0, vecc.size() - 1) {
			vecc[i].normalize();
			save(as, i + 1, vecc[i]);
		}
	}
}
/*
void SVD(matrix x,matrix &s,matrix &v,matrix &d) {
	matrix tmp;
	tmp=x.transposition()*x;
	find_T(tmp,d);
	tmp=d.transposition()*x*d;
	int n=tmp.rank();
	v=x;
	v.clear();
	rep(i,1,n) v.num[i][i]=tmp.num[i][i];
}
*/
