import time
import pandas as pd
import requests
from bs4 import BeautifulSoup
import re
import os

def get_all_port_urls(base_url):
    """
    从指定 URL 抓取所有港口链接。

    参数:
    base_url (str): 包含港口列表的页面的 URL。

    返回:
    list: 包含每个港口名称和 URL 的字典列表。
    """
    headers = {
        'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36'
    }

    try:
        response = requests.get(base_url, headers=headers, timeout=100)
        response.raise_for_status() # 如果请求失败，抛出HTTPError
        html_content = response.text
    except requests.exceptions.RequestException as e:
        print(f"请求失败: {e}")
        return []

    soup = BeautifulSoup(html_content, 'html.parser')
    
    data_list = []
    
    # 找到包含港口列表的 ul 标签
    port_table_div = soup.find('div', class_='port-table')
    # 如果找到，可以进一步处理，比如提取里面的表格
    if port_table_div:
        table = port_table_div.find('table')
        # 处理表格数据...
        # 例如，提取所有行
        rows = table.find_all('tr')
        for row in rows:
            td_elements = row.find_all('td')

                
            # 确保有足够的列（至少5列）
            if len(td_elements) >= 5:
                # 提取链接信息
                link_element = td_elements[0].find('a')
                link_text = link_element.text.strip() if link_element else ""
                link_href = link_element['href'] if link_element and link_element.has_attr('href') else ""  
                # 创建数据字典
                row_data = {
                    "main_port": True if td_elements[0].find('span') else False,
                    "link_text": link_text,
                    "link_href": link_href,
                    "region": td_elements[1].text.strip(),
                    "code": td_elements[2].text.strip(),
                    "country": td_elements[3].text.strip(),
                    "port_type": td_elements[4].text.strip()
                    }
                # 添加到数据列表
                data_list.append(row_data)
    else:
        print("未找到class为'port-table'的div")    
    
    df = pd.DataFrame(data_list)
            
    return df



def fetch_and_parse_port_data(url):
    """
    从指定网页抓取并解析港口信息。

    参数:
    url (str): 目标网页的URL。

    返回:
    dict: 包含提取出的港口信息的字典。如果抓取或解析失败，返回一个空字典。
    """
    headers = {
        'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36'
    }

    try:
        response = requests.get(url, headers=headers, timeout=10)
        response.raise_for_status()  # 如果请求失败，抛出HTTPError
        html_content = response.text
    except requests.exceptions.RequestException as e:
        print(f"请求失败: {e}")
        return {}

    soup = BeautifulSoup(html_content, 'html.parser')
    port_info = {}

    # 1. 从 <ul> 列表中提取信息
    ul_list = soup.find('ul', class_='port-items')
    if ul_list:
        for li in ul_list.find_all('li'):
            span_text = li.find('span')
            if span_text:
                key = span_text.text.strip().replace('：', '')
                value = span_text.next_sibling
                
                if key == '港口中文':
                    port_info['chinese_name'] = value
                elif key == '港口英文':
                    port_info['english_name'] = value
                elif key == '港口代码':
                    port_info['port_code'] = value
                elif key == '所属国家':
                    port_info['country'] = value
                elif key == '国家英文':
                    port_info['country_english'] = value

    # 2. 从 <p> 段落中提取经纬度
    p_tag = soup.find('p')
    if p_tag:
        p_text = p_tag.text
        # 使用正则表达式匹配经度和纬度
        lat_match = re.search(r'纬度：([\d.-]+)', p_text)
        lon_match = re.search(r'经度([\d.-]+)', p_text)
        
        if lat_match:
            port_info['latitude'] = float(lat_match.group(1))
        if lon_match:
            port_info['longitude'] = float(lon_match.group(1))

    return port_info

def supplement_missing_coordinates(df, manual_coords):
    """
    补充DataFrame中缺失的港口经纬度。

    参数:
    df (pd.DataFrame): 包含港口数据的DataFrame。
    manual_coords (dict): 包含手动补充经纬度的字典，
                          键为港口名称 (region)，值为包含 'latitude' 和 'longitude' 的字典。

    返回:
    pd.DataFrame: 补充后的DataFrame。
    """
    print("\n开始补充缺失的经纬度...")
    updated_count = 0
    for index, row in df.iterrows():
        # 检查经纬度是否缺失
        if pd.isna(row['latitude']) or pd.isna(row['longitude']):
            port_name = row['region'] # 使用link_text作为查找键
            if port_name in manual_coords:
                new_lat = manual_coords[port_name].get('latitude')
                new_lon = manual_coords[port_name].get('longitude')
                
                # 仅当手动值有效时才更新
                if new_lat is not None and new_lon is not None:
                    df.at[index, 'latitude'] = new_lat
                    df.at[index, 'longitude'] = new_lon
                    updated_count += 1
                    print(f"  已补充 '{port_name}' 的经纬度: ({new_lat}, {new_lon})")
    print(f"经纬度补充完成，共更新了 {updated_count} 条记录。")
    return df

if __name__ == "__main__":
    
    url = "https://port.yunfei89.com/country45/"
    
    link_port = get_all_port_urls(url)
    
    # 检查是否成功获取数据
    if link_port.empty:
        print("未获取到港口链接数据")
    else:
        print(f"成功获取 {len(link_port)} 个港口链接")
        
        # 创建空列表存储解析结果
        port_details = []
        
        # 遍历每个链接
        for index, row in link_port.iterrows():
            port_url = row['link_href']
            print(f"正在处理 {index+1}/{len(link_port)}: {port_url}")
            
            # 获取港口详细信息
            port_info = fetch_and_parse_port_data(port_url)
            
            # 添加基础信息
            # port_info['link_text'] = row['link_text']
            # port_info['region'] = row['region']
            # port_info['code'] = row['code']
            # port_info['country'] = row['country']
            # port_info['port_type'] = row['port_type']
            
            # 添加到结果列表
            port_details.append(port_info)
            
            # 添加延迟以避免被封
            time.sleep(1)  # 1秒延迟
        
        # 创建详细信息的DataFrame
        details_df = pd.DataFrame(port_details)
        
        # 合并到原始DataFrame
        # 使用索引合并，确保顺序一致
        final_df = pd.concat([link_port, details_df], axis=1)
        # 手动补充港口经纬度
        manual_coordinates = {
            "连云港": {"latitude": 34.765904, "longitude": 119.429225},
            "八所港": {"latitude": 19.120822, "longitude": 108.654330},
            "四会": {"latitude": 23.224257, "longitude": 112.825967},
            "阳江": {"latitude": 21.717534, "longitude": 111.825966},
            # 在这里添加更多需要手动补充的港口数据
            # "其他港口名称": {"latitude": 12.345, "longitude": 67.890},
        }        
        final_df = supplement_missing_coordinates(final_df, manual_coordinates)

        # 保存结果
        # 定义输出目录
        output_dir = "data/"
        output_file = os.path.join(output_dir, "all_ports_data.csv")
    
        # 确保目录存在
        os.makedirs(output_dir, exist_ok=True)
    
        # 保存结果
        final_df.to_csv(output_file, index=False, encoding='utf-8-sig')

        # print(f"数据已保存到 {output_file}，共 {len(final_df)} 条记录")        print(f"数据已保存到 all_ports_data.csv，共 {len(final_df)} 条记录")
        
        # 显示前几行数据
        print("\n数据预览:")
        print(final_df.head())
