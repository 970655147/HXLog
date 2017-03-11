/**
 * file name : Test01JsonExtractor.java
 * created at : 下午6:22:03 2016年8月11日
 * created by 970655147
 */

package com.hx.log.test;

import static com.hx.log.util.Log.info;
import static com.hx.log.util.Log.log;

import java.util.BitSet;

import org.junit.Test;

import com.hx.log.util.Eval;
import com.hx.log.util.interf.IdxIterator;
import com.hx.log.util.interf.IdxIterator.SomeBitIncIdxIterator;
import com.hx.log.util.JSONExtractor;

import net.sf.json.JSONArray;
import net.sf.json.JSONObject;

public class Test01_JsonExtractor {

	// 根据给定的表达式, 提取给定的对象中的数据
	public static void main(String[] args) {
	
		
	}
	
	@Test
	public void testEval() {
		// test ;
//		log(Eval.eval("abc") );
//		log(Eval.eval("??2+12") );
		log(Eval.eval("1 + ((1 * 5)) + 5") );
	}
	
	@Test
	public void testPrepare() {
//		String str = "????";
		String str = "????*";
//		String str = "????**";
//		String str = "????**12*";
		log(JSONExtractor.preparePattern(str) );
	}
	
	@Test 
	public void testIdxIterator() {
////	IdxIterator ite = new SingleIdxIterator(2);
////	IdxIterator ite = new RangeIdxIterator(2, 12);
////	IdxIterator ite = Inc.newCntInc(1, 2, 3);
//		IdxIterator ite = Inc.newEndInc(1, 2, 9);
		BitSet bs = new BitSet();
			bs.set(0);
			bs.set(2);
		IdxIterator ite = new SomeBitIncIdxIterator(20, 120, bs);
		while(ite.hasNext() ) {
			log(ite.next() );
		}
	}
	
	@Test
	public void getIdxIteratorByPattern0() {
		IdxIterator ite = JSONExtractor.getIdxIteratorByPattern("???2", 123);
		while(ite.hasNext() ) {
			log(ite.next() );
		}
	}
	
	@Test
	public void testExtractJson() {
		
//		String pattern = "$this[1, 4].category.url";
//		String pattern = "$this['?2?', '?3?'].category.url";
//		String pattern = "$this[?2?, ?3?].category.url";
//		String pattern = "$this.category[*].url";
//		String pattern = "$this.category[0,2]";
//		JSONObject obj = new JSONObject().element("category", new JSONArray().element(new JSONObject().element("url", "http://www.abidu.com")).element(new JSONObject().element("url", "http://www.hx.com")) );
		
//		String pattern = "$this.page-1[*].products[0, 10].title";
//		String pattern = "$this.page-1[*].products[1, ?].title";
//		String pattern = "$this.page-1[*].products[??].title";
//		JSONObject obj = JSONObject.fromObject("{\"page-1\":[{\"products\":[{\"title\":\"08 两个线程交替打印121212...\",\"date\":\"2016-08-09 21:43\",\"view\":\"(8)\"},{\"title\":\"17 FileNameMatcher\",\"date\":\"2016-08-07 21:20\",\"view\":\"(12)\"},{\"title\":\"16 pointFixLike\",\"date\":\"2016-05-21 22:00\",\"view\":\"(1841)\"},{\"title\":\"一件令人蛋疼的事\",\"date\":\"2016-04-09 23:08\",\"view\":\"(89)\"},{\"title\":\"15 几个Calender相关的方法\",\"date\":\"2016-03-03 21:48\",\"view\":\"(103)\"},{\"title\":\"08 scala, imported `Record' is permanently hidden by definition of class Record in package test\",\"date\":\"2016-03-01 20:57\",\"view\":\"(161)\"},{\"title\":\"01 SparkStreaming's WordCount\",\"date\":\"2016-02-12 21:49\",\"view\":\"(211)\"},{\"title\":\"14 screenShotLikeQQ\",\"date\":\"2016-02-02 20:53\",\"view\":\"(121)\"},{\"title\":\"13 gifGenerator\",\"date\":\"2016-02-01 20:36\",\"view\":\"(146)\"},{\"title\":\"12 添加水印\",\"date\":\"2016-01-27 21:08\",\"view\":\"(100)\"},{\"title\":\"11 绘制雪花动态图\",\"date\":\"2016-01-26 20:31\",\"view\":\"(102)\"},{\"title\":\"07 八皇后问题\",\"date\":\"2016-01-26 20:08\",\"view\":\"(104)\"},{\"title\":\"10 绘制数字\",\"date\":\"2016-01-25 21:05\",\"view\":\"(92)\"},{\"title\":\"30 从n个数中随机获取m个数字\",\"date\":\"2016-01-24 20:44\",\"view\":\"(387)\"},{\"title\":\"29 同位词的统计\",\"date\":\"2016-01-23 20:33\",\"view\":\"(89)\"}]}],\"page-2\":[{\"products\":[{\"title\":\"28 找出不存在的数字\",\"date\":\"2016-01-23 10:21\",\"view\":\"(107)\"},{\"title\":\"27 电话号码排序\",\"date\":\"2016-01-20 21:22\",\"view\":\"(102)\"},{\"title\":\"26 parseInt\",\"date\":\"2016-01-20 20:24\",\"view\":\"(97)\"},{\"title\":\"25 不使用加减乘除做加法\",\"date\":\"2016-01-18 21:08\",\"view\":\"(84)\"},{\"title\":\"24 约瑟夫环\",\"date\":\"2016-01-17 20:51\",\"view\":\"(84)\"},{\"title\":\"23 判断扑克牌的顺子\",\"date\":\"2016-01-17 20:42\",\"view\":\"(73)\"},{\"title\":\"22 k个骰子掷出n的概率\",\"date\":\"2016-01-16 21:03\",\"view\":\"(85)\"},{\"title\":\"21 反转句子顺序\",\"date\":\"2016-01-16 20:32\",\"view\":\"(88)\"},{\"title\":\"20 找出第1500个丑数\",\"date\":\"2016-01-15 21:09\",\"view\":\"(73)\"},{\"title\":\"19 获取给定的数组能够组成的最小的数字\",\"date\":\"2016-01-15 10:47\",\"view\":\"(73)\"},{\"title\":\"18 获取给定的序列的所有排列, 组合\",\"date\":\"2016-01-15 10:00\",\"view\":\"(81)\"},{\"title\":\"17 将二叉排序树转换为有序双链表\",\"date\":\"2016-01-12 21:11\",\"view\":\"(72)\"},{\"title\":\"16 复杂链表的复制\",\"date\":\"2016-01-12 20:55\",\"view\":\"(73)\"},{\"title\":\"15 打印给定的二叉树中和为给定的值的路径\",\"date\":\"2016-01-11 21:06\",\"view\":\"(66)\"},{\"title\":\"14 判断给定的二叉排序树后序遍历序列是否合法\",\"date\":\"2016-01-11 20:45\",\"view\":\"(65)\"}]}],\"page-3\":[{\"products\":[{\"title\":\"13 给定的出栈序列是否满足入栈序列\",\"date\":\"2016-01-09 21:19\",\"view\":\"(80)\"},{\"title\":\"12 蛇形打印矩阵中的数据\",\"date\":\"2016-01-09 21:01\",\"view\":\"(89)\"},{\"title\":\"11 是否存在子树 & 二叉树镜像\",\"date\":\"2016-01-08 20:33\",\"view\":\"(90)\"},{\"title\":\"10 归并两个有序链表\",\"date\":\"2016-01-08 19:47\",\"view\":\"(91)\"},{\"title\":\"09 链表中找出倒数第k个数 & 找出链表正中间的数据\",\"date\":\"2016-01-01 21:05\",\"view\":\"(72)\"},{\"title\":\"08 将奇数排在偶数之前\",\"date\":\"2015-12-31 21:18\",\"view\":\"(66)\"},{\"title\":\"07 打印1到最大的n位数\",\"date\":\"2015-12-31 20:59\",\"view\":\"(98)\"},{\"title\":\"06 实现数值的整数次方\",\"date\":\"2015-12-30 21:21\",\"view\":\"(72)\"},{\"title\":\"05 旋转数组中的最小数字\",\"date\":\"2015-12-30 20:20\",\"view\":\"(65)\"},{\"title\":\"04 两个栈构造队列 & 两个队列构造栈\",\"date\":\"2015-12-29 21:26\",\"view\":\"(69)\"},{\"title\":\"03 给定链表头结点, 依次输出从尾节点到头结点的数据\",\"date\":\"2015-12-29 20:42\",\"view\":\"(72)\"},{\"title\":\"02 替换' '为\\\"%32\\\"\",\"date\":\"2015-12-28 22:46\",\"view\":\"(57)\"},{\"title\":\"01 在递增[下面的<上面的, 左边的<右边的]二维数组中查找给定的值\",\"date\":\"2015-12-28 21:50\",\"view\":\"(80)\"},{\"title\":\"06 RedBlackTree\",\"date\":\"2015-12-28 20:10\",\"view\":\"(73)\"},{\"title\":\"07 eclipse C/ C++编译含有多个main函数的项目\",\"date\":\"2015-12-23 21:50\",\"view\":\"(194)\"}]}],\"page-4\":[{\"products\":[{\"title\":\"09 DirectoryStructure\",\"date\":\"2015-12-21 21:26\",\"view\":\"(91)\"},{\"title\":\"06 环形矩阵\",\"date\":\"2015-12-14 22:23\",\"view\":\"(132)\"},{\"title\":\"05 简易计算器\",\"date\":\"2015-12-14 22:11\",\"view\":\"(71)\"},{\"title\":\"04 计算平台数\",\"date\":\"2015-12-14 22:08\",\"view\":\"(58)\"},{\"title\":\"05 BinarySortTree\",\"date\":\"2015-12-14 10:50\",\"view\":\"(76)\"},{\"title\":\"04 BloomFilter\",\"date\":\"2015-12-04 21:05\",\"view\":\"(95)\"},{\"title\":\"06 maven问题 \\\"Could not calculate build plan: Plugin 'jarPrefix':'jarName':'jarVersion' \\\"\",\"date\":\"2015-12-03 21:16\",\"view\":\"(250)\"},{\"title\":\"02 第一份实习工作辞职之后的三个月\",\"date\":\"2015-12-02 22:10\",\"view\":\"(101)\"},{\"title\":\"08 SimpleServer\",\"date\":\"2015-11-27 21:27\",\"view\":\"(124)\"},{\"title\":\"03 MyBitSet\",\"date\":\"2015-11-27 16:23\",\"view\":\"(92)\"},{\"title\":\"15 java.util.BitSet\",\"date\":\"2015-11-21 20:42\",\"view\":\"(108)\"},{\"title\":\"02 计算字符串表达式 [类似于js eval函数]\",\"date\":\"2015-11-21 15:43\",\"view\":\"(88)\"},{\"title\":\"07 java.awt.Robot的一些使用\",\"date\":\"2015-11-18 20:57\",\"view\":\"(179)\"},{\"title\":\"0602 播客框架分析\",\"date\":\"2015-11-16 17:31\",\"view\":\"(106)\"},{\"title\":\"0601 生成验证码\",\"date\":\"2015-11-16 13:28\",\"view\":\"(77)\"}]}],\"page-5\":[{\"products\":[{\"title\":\"10 2048\",\"date\":\"2015-11-15 21:58\",\"view\":\"(85)\"},{\"title\":\"05 ueditor上传图片配置\",\"date\":\"2015-11-15 17:21\",\"view\":\"(90)\"},{\"title\":\"0103 混蛋罗心得[装*技巧]\",\"date\":\"2015-11-12 12:34\",\"view\":\"(105)\"},{\"title\":\"0102 混蛋罗心得[敌人方位(多图, 慎入)]\",\"date\":\"2015-11-11 22:56\",\"view\":\"(93)\"},{\"title\":\"0101 混蛋罗心得[基本元素]\",\"date\":\"2015-11-11 20:30\",\"view\":\"(100)\"},{\"title\":\"06 最近做的一个播客站点, 请大家没事的时候'玩儿'一下\",\"date\":\"2015-11-10 16:02\",\"view\":\"(256)\"},{\"title\":\"04 bootstrap \\\"模态框跳转到当前模板页面, 框消失了, 而背景依然存在问题\\\"\",\"date\":\"2015-11-10 14:40\",\"view\":\"(339)\"},{\"title\":\"03 发布项目到tomcat但是.java未编译问题\",\"date\":\"2015-11-04 16:50\",\"view\":\"(126)\"},{\"title\":\"02 bootstrap \\\"modal : is not a function\\\"\",\"date\":\"2015-11-03 13:30\",\"view\":\"(422)\"},{\"title\":\"09 智慧桥/ 艾摩君\",\"date\":\"2015-10-28 20:58\",\"view\":\"(190)\"},{\"title\":\"01 筛选法求素数\",\"date\":\"2015-10-25 22:14\",\"view\":\"(117)\"},{\"title\":\"01 反射参数问题\",\"date\":\"2015-10-24 19:03\",\"view\":\"(84)\"},{\"title\":\"05 代码片格式化\",\"date\":\"2015-10-22 21:25\",\"view\":\"(123)\"},{\"title\":\"04 eclipse 自动补全功能的改进\",\"date\":\"2015-10-14 10:58\",\"view\":\"(122)\"},{\"title\":\"14 java.util.LinkedHashMap\",\"date\":\"2015-10-07 21:50\",\"view\":\"(134)\"}]}],\"page-6\":[{\"products\":[{\"title\":\"13 java.util.HashMap\",\"date\":\"2015-10-06 19:41\",\"view\":\"(112)\"},{\"title\":\"12 java.util.LinkedList\",\"date\":\"2015-10-05 19:29\",\"view\":\"(133)\"},{\"title\":\"11 java.util.ArrayList\",\"date\":\"2015-10-04 17:24\",\"view\":\"(169)\"},{\"title\":\"03crawler02 爬取贴吧排名, 制作图片集\",\"date\":\"2015-10-02 20:48\",\"view\":\"(141)\"},{\"title\":\"03crawler01 爬取直播电视剧列表\",\"date\":\"2015-10-02 19:41\",\"view\":\"(113)\"},{\"title\":\"03 crawler\",\"date\":\"2015-10-02 16:21\",\"view\":\"(184)\"},{\"title\":\"02 transferTools\",\"date\":\"2015-10-01 19:28\",\"view\":\"(125)\"},{\"title\":\"01 程序启动工具\",\"date\":\"2015-10-01 15:46\",\"view\":\"(96)\"},{\"title\":\"08 连连看\",\"date\":\"2015-09-30 10:55\",\"view\":\"(118)\"},{\"title\":\"10 java.lang.ThreadLocal\",\"date\":\"2015-09-29 14:38\",\"view\":\"(119)\"},{\"title\":\"07 扫雷\",\"date\":\"2015-09-28 16:19\",\"view\":\"(129)\"},{\"title\":\"4.9 数独问题\",\"date\":\"2015-09-26 21:22\",\"view\":\"(122)\"},{\"title\":\"4.7 蚂蚁爬杆问题\",\"date\":\"2015-09-18 22:40\",\"view\":\"(119)\"},{\"title\":\"09 java.lang.Runtime\",\"date\":\"2015-09-13 12:53\",\"view\":\"(108)\"},{\"title\":\"4.4 给定的点是否在三角形之内\",\"date\":\"2015-09-10 21:49\",\"view\":\"(117)\"}]}],\"page-7\":[{\"products\":[{\"title\":\"3.8 二叉树中结点最大的距离 & 重建二叉树 & 顺序遍历二叉树\",\"date\":\"2015-09-05 19:55\",\"view\":\"(125)\"},{\"title\":\"08 java.lang.ProcessBuilder\",\"date\":\"2015-08-29 16:27\",\"view\":\"(145)\"},{\"title\":\"3.7 队列的最大值问题\",\"date\":\"2015-08-28 22:03\",\"view\":\"(122)\"},{\"title\":\"06 黑白棋\",\"date\":\"2015-08-28 16:12\",\"view\":\"(127)\"},{\"title\":\"3.6 判断两个无环链表是否相交 & 找出相交的第一个结点\",\"date\":\"2015-08-28 15:05\",\"view\":\"(116)\"},{\"title\":\"3.4 从无头链表中删除给定的结点 & 遍历一次逆转链表\",\"date\":\"2015-08-28 14:32\",\"view\":\"(121)\"},{\"title\":\"3.2 电话号码对应的英语单词\",\"date\":\"2015-08-27 18:24\",\"view\":\"(85)\"},{\"title\":\"07 java.lang.StringBuilder\",\"date\":\"2015-08-26 22:43\",\"view\":\"(119)\"},{\"title\":\"06 java.lang.AbstractStringBuilder\",\"date\":\"2015-08-26 22:20\",\"view\":\"(113)\"},{\"title\":\"3.1 字符串移位包含问题\",\"date\":\"2015-08-26 21:51\",\"view\":\"(109)\"},{\"title\":\"2.19 区间重合判定\",\"date\":\"2015-08-26 21:30\",\"view\":\"(120)\"},{\"title\":\"05 java.lang.String\",\"date\":\"2015-08-25 22:39\",\"view\":\"(131)\"},{\"title\":\"2.18 数组的分割\",\"date\":\"2015-08-25 21:36\",\"view\":\"(99)\"},{\"title\":\"2.17 数组循环移位\",\"date\":\"2015-08-25 20:31\",\"view\":\"(100)\"},{\"title\":\"04 java.lang.Byte\",\"date\":\"2015-08-24 22:44\",\"view\":\"(128)\"}]}],\"page-8\":[{\"products\":[{\"title\":\"2.16 求一个数列的最长递增子序列\",\"date\":\"2015-08-24 22:25\",\"view\":\"(140)\"},{\"title\":\"03 java.lang.Boolean\",\"date\":\"2015-08-20 21:13\",\"view\":\"(114)\"},{\"title\":\"2.15 求二维数组的子矩阵的最大和\",\"date\":\"2015-08-20 20:58\",\"view\":\"(125)\"},{\"title\":\"2.14 子数组之和的最大值\",\"date\":\"2015-08-20 20:03\",\"view\":\"(140)\"},{\"title\":\"02 java.lang.Integer\",\"date\":\"2015-08-19 21:39\",\"view\":\"(125)\"},{\"title\":\"2.13 子数组的最大乘积\",\"date\":\"2015-08-19 21:05\",\"view\":\"(122)\"},{\"title\":\"2.12 找出数组中和为给定的值的两个数字\",\"date\":\"2015-08-19 20:31\",\"view\":\"(108)\"},{\"title\":\"00 声明\",\"date\":\"2015-08-18 21:17\",\"view\":\"(119)\"},{\"title\":\"01 java.lang.Object\",\"date\":\"2015-08-18 21:13\",\"view\":\"(118)\"},{\"title\":\"2.10 寻找数组中的最大值和最小值\",\"date\":\"2015-08-18 20:39\",\"view\":\"(118)\"},{\"title\":\"2.9 Fibonacci数列\",\"date\":\"2015-08-18 20:09\",\"view\":\"(102)\"},{\"title\":\"2.8 找出整除n的只有0, 1的数\",\"date\":\"2015-08-17 22:17\",\"view\":\"(108)\"},{\"title\":\"2.8 寻找符合条件的数\",\"date\":\"2015-08-17 22:09\",\"view\":\"(109)\"},{\"title\":\"05 魔方\",\"date\":\"2015-08-17 19:08\",\"view\":\"(150)\"},{\"title\":\"2.6/ 7 精确表示浮点数 + 最大公约数\",\"date\":\"2015-08-16 18:39\",\"view\":\"(114)\"}]}],\"page-9\":[{\"products\":[{\"title\":\"2.5 寻找最大的k个数\",\"date\":\"2015-08-15 21:08\",\"view\":\"(112)\"},{\"title\":\"2.4 1的数目\",\"date\":\"2015-08-12 21:25\",\"view\":\"(118)\"},{\"title\":\"04 迷宫寻路\",\"date\":\"2015-08-11 21:31\",\"view\":\"(154)\"},{\"title\":\"2.3 寻找\\\"水贴王\\\"\",\"date\":\"2015-08-11 20:38\",\"view\":\"(200)\"},{\"title\":\"2.2 阶乘中的问题\",\"date\":\"2015-08-11 20:13\",\"view\":\"(112)\"},{\"title\":\"03 推箱子\",\"date\":\"2015-08-10 20:54\",\"view\":\"(136)\"},{\"title\":\"2.1 二进制中1的个数\",\"date\":\"2015-08-10 19:30\",\"view\":\"(126)\"},{\"title\":\"1.5 快速找出故障的机器\",\"date\":\"2015-08-09 21:07\",\"view\":\"(147)\"},{\"title\":\"1.3 翻烙饼问题\",\"date\":\"2015-08-09 19:46\",\"view\":\"(193)\"},{\"title\":\"1.2 将帅问题\",\"date\":\"2015-08-09 16:29\",\"view\":\"(156)\"},{\"title\":\"02 俄罗斯方块\",\"date\":\"2015-08-09 14:08\",\"view\":\"(159)\"},{\"title\":\"01 贪吃蛇\",\"date\":\"2015-08-08 21:48\",\"view\":\"(178)\"},{\"title\":\"01 开篇\",\"date\":\"2015-08-08 21:14\",\"view\":\"(147)\"},{\"title\":\"sessionFactory.openSession().save(Obj)，只执行了select max (id) from XXtable问题\",\"date\":\"2014-08-06 18:16\",\"view\":\"(352)\"},{\"title\":\"hibernate, Bad format for DATE ‘1’ in column 4的问题\",\"date\":\"2014-08-03 18:36\",\"view\":\"(1174)\"}]}],\"page-10\":[{\"products\":[{\"title\":\"第三个程序：生成两个个ffH以内的随机数，并选择随机的运算符计算[但是好像非随机吧，cpu太快了]\",\"date\":\"2014-07-27 15:44\",\"view\":\"(359)\"},{\"title\":\"第一个程序：Hello World\",\"date\":\"2014-07-27 15:39\",\"view\":\"(356)\"},{\"title\":\"第二个程序：判断闰年\",\"date\":\"2014-07-27 15:38\",\"view\":\"(353)\"}]}]}");
//		String pattern = "$this[0,3].title";
//		String pattern = "$this[0,3]";
//		String pattern = "$this[1?]";
//		String pattern = "$this['eval#1+1', eval#$len]";
//		String pattern = "$this[1?]";
//		JSONArray obj = JSONArray.fromObject("[{\"title\":\"08两个线程交替打印121212...\",\"date\":\"2016-08-0921:43\",\"view\":\"(8)\"},{\"title\":\"17FileNameMatcher\",\"date\":\"2016-08-0721:20\",\"view\":\"(12)\"},{\"title\":\"16pointFixLike\",\"date\":\"2016-05-2122:00\",\"view\":\"(1841)\"},{\"title\":\"一件令人蛋疼的事\",\"date\":\"2016-04-0923:08\",\"view\":\"(89)\"},{\"title\":\"15几个Calender相关的方法\",\"date\":\"2016-03-0321:48\",\"view\":\"(103)\"},{\"title\":\"08scala,imported`Record'ispermanentlyhiddenbydefinitionofclassRecordinpackagetest\",\"date\":\"2016-03-0120:57\",\"view\":\"(161)\"},{\"title\":\"01SparkStreaming'sWordCount\",\"date\":\"2016-02-1221:49\",\"view\":\"(211)\"},{\"title\":\"14screenShotLikeQQ\",\"date\":\"2016-02-0220:53\",\"view\":\"(121)\"},{\"title\":\"13gifGenerator\",\"date\":\"2016-02-0120:36\",\"view\":\"(146)\"},{\"title\":\"12添加水印\",\"date\":\"2016-01-2721:08\",\"view\":\"(100)\"},{\"title\":\"11绘制雪花动态图\",\"date\":\"2016-01-2620:31\",\"view\":\"(102)\"},{\"title\":\"07八皇后问题\",\"date\":\"2016-01-2620:08\",\"view\":\"(104)\"},{\"title\":\"10绘制数字\",\"date\":\"2016-01-2521:05\",\"view\":\"(92)\"},{\"title\":\"30从n个数中随机获取m个数字\",\"date\":\"2016-01-2420:44\",\"view\":\"(387)\"},{\"title\":\"29同位词的统计\",\"date\":\"2016-01-2320:33\",\"view\":\"(89)\"}]");
		
		
//		String pattern = "$this.pageSum";
//		String pattern = "$this.curTag";
//		String pattern = "$this.tagList[4].text";
//		String pattern = "$this.tagList[?].text";
//		String pattern = "$this.blogList[*].tags";
//		String pattern = "$this.blogList[*].tags[eval#$len/2]";
		String pattern = "$this.sdf|$this.blogList[*].tags[eval#$len/2]";
		
		JSONObject obj = JSONObject.fromObject("{\"tagList\":[{\"text\":\"all\",\"cnt\":8},{\"text\":\"java\",\"cnt\":7},{\"text\":\"lock\",\"cnt\":4},{\"text\":\"字符串匹配\",\"cnt\":3},{\"text\":\"cpp\",\"cnt\":1},{\"text\":\"eclipse\",\"cnt\":1},{\"text\":\"mingw\",\"cnt\":1}],\"blogList\":[{\"id\":16,\"title\":\"Boyer-Moore算法\",\"tags\":[\"字符串匹配\",\"java\"],\"date\":\"2016-01-3121:10\",\"good\":2,\"notGood\":1,\"visited\":41,\"commentsNum\":9},{\"id\":15,\"title\":\"KMP算法\",\"tags\":[\"字符串匹配\",\"java\"],\"date\":\"2016-01-3120:33\",\"good\":1,\"notGood\":1,\"visited\":9,\"commentsNum\":3},{\"id\":14,\"title\":\"自动机字符串匹配\",\"tags\":[\"字符串匹配\",\"java\"],\"date\":\"2016-01-3120:12\",\"good\":0,\"notGood\":1,\"visited\":5,\"commentsNum\":1},{\"id\":13,\"title\":\"eclipseC&CPP编译含有多个main函数的项目\",\"tags\":[\"eclipse\",\"cpp\",\"mingw\"],\"date\":\"2015-12-2816:02\",\"good\":1,\"notGood\":0,\"visited\":6,\"commentsNum\":1},{\"id\":11,\"title\":\"Java锁的种类以及辨析(四)可重入锁\",\"tags\":[\"java\",\"lock\"],\"date\":\"2015-12-2521:59\",\"good\":0,\"notGood\":1,\"visited\":3,\"commentsNum\":0},{\"id\":10,\"title\":\"Java锁的种类以及辨析(三)阻塞锁\",\"tags\":[\"java\",\"lock\"],\"date\":\"2015-12-2521:58\",\"good\":1,\"notGood\":0,\"visited\":3,\"commentsNum\":0},{\"id\":9,\"title\":\"Java锁的种类以及辨析(二)自旋锁的其他种类\",\"tags\":[\"java\",\"lock\"],\"date\":\"2015-12-2521:58\",\"good\":0,\"notGood\":1,\"visited\":3,\"commentsNum\":1},{\"id\":8,\"title\":\"java锁的种类以及辨析(一)自旋锁\",\"tags\":[\"java\",\"lock\"],\"date\":\"2015-12-2521:56\",\"good\":0,\"notGood\":0,\"visited\":5,\"commentsNum\":0}],\"pageSum\":1,\"curTag\":\"all\"}");
		
		JSONArray res = JSONExtractor.extractInfoFromJSON(obj, pattern);
		info(res.toString() );
	}
	
}
